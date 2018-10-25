package mimo

import chisel3._
import chisel3.experimental.FixedPoint
import chisel3.util.Decoupled
import chisel3.util.log2Ceil
import dsptools.numbers._
import freechips.rocketchip.diplomacy.LazyModule
import freechips.rocketchip.subsystem.BaseSubsystem
import breeze.math.Complex

/**
  * In the pilot phase, the FFTFSM takes spectrums from the FFT consisting of S subcarriers.
  * There are K users in the system, and they will send staggered BPSK pilots.
  * Therefore, each FSM is a K x S dimensional system. and there are M FSMs (M antennas).
  * The FSM uses the known pilots per user and the spectrums to find the Hermitian conjugate.
  * Each K x S Hermitian conjugate matrix is the weights of one slice of the M x K x S matrix multiplier.
  * Finally, in the payload phase, the M x K x S weights times the M x S spectrums gives a K x S matrix (final spectrum for all K users.
  * The FSM can replace any weight in the system from an external memory mapped interface during the payload phase.
  */

/**
 * Base class for FFTFSM parameters
 *
 * These are type generic
 */
trait FFTFSMParams[T <: Data] {
  // DspComplex
  val proto: DspComplex[T]
  // Number of subcarriers
  val S: Int
  // Number of UEs (pilot frames)
  val K: Int
  // Number of antennas
  val M: Int
  // Number of payload frames
  val F: Int
  // Oversampling ratio
  val O: Int
}

/**
 * FFTFSM parameters object for fixed-point FFTFSMs
 */
case class FixedFFTFSMParams(
  // width of input spectrum & output weights
  IOWidth: Int,
  S: Int,
  K: Int,
  M: Int,
  F: Int,
  O: Int,
) extends FFTFSMParams[FixedPoint] {
  // 1 sign & 1 integer bit since range is -1 to +1
  val proto = DspComplex(FixedPoint(IOWidth.W, (IOWidth-2).BP), FixedPoint(IOWidth.W, (IOWidth-2).BP))
}

/**
  * To override weight from AXI4. Valid pulse implies weight needs to be overridden.
  */
class ExtWtBundle[T <: Data](params: FFTFSMParams[T]) extends Bundle {
  val kAddr = UInt(log2Ceil(params.K).W)
  val sAddr = UInt(log2Ceil(params.S).W)
  val wt = params.proto.cloneType

  override def cloneType: this.type = ExtWtBundle(params).asInstanceOf[this.type]

}
object ExtWtBundle {
  def apply[T <: Data](params: FFTFSMParams[T]): ExtWtBundle[T] = new ExtWtBundle(params)
}

/**
 * Bundle type as IO for FFTFSM modules
 */
class FFTFSMIO[T <: Data](params: FFTFSMParams[T]) extends Bundle {
  // FFT spectrum
  val in = Flipped(Decoupled(Vec(params.S, params.proto.cloneType)))
  // Matrix weights
  val out = Vec(params.K, Decoupled(Vec(params.S, params.proto.cloneType)))
  // Known pilots
  val pilots = Flipped(Decoupled(Vec(params.K, Vec(params.S, Bool()))))
  // Set weights externally
  val extWt = Flipped(Decoupled(new ExtWtBundle(params)))

  override def cloneType: this.type = FFTFSMIO(params).asInstanceOf[this.type]
}
object FFTFSMIO {
  def apply[T <: Data](params: FFTFSMParams[T]): FFTFSMIO[T] =
    new FFTFSMIO(params)
}

class FFTFSM[T <: Data : Real](val params: FFTFSMParams[T]) extends Module {
  // check parameters
  require(params.S > 0)
  require(params.K > 0)
  require(params.M > 0)
  require(params.F > 0)
  require(params.O > 0)

  val io = IO(FFTFSMIO(params))

  // SETUP - regs

  // known pilots
  val pReg = Reg(Vec(params.K, Vec(params.S, Bool())))
  // spectrum
  val sReg = Reg(Vec(params.S, params.proto.cloneType))
  // weights
  // TODO: make this into memory?
  val wMem = Reg(Vec(params.K, Vec(params.S, params.proto.cloneType)))

  // pilot counter
  val kCnt = RegInit(0.U(log2Ceil(params.K).W))
  // payload counter
  val fCnt = RegInit(0.U(log2Ceil(params.F).W))

  // output valids
  val outValidReg = RegInit(Vec(Seq.fill(params.K)(false.B)))


  // COMBINATORIAL CALCULATION

  // scale down by # of antennas so that matrix mult sum = 1
  // TODO: this isn't type generic. Somehow can't get it to convert to params.proto.
  val scale = DspComplex(ConvertableTo[T].fromDouble(1.0/params.M), ConvertableTo[T].fromDouble(0))
  //val scale = params.proto(Complex(1.0/params.M, 0))
  // channel estimate = conjugate (subcarrier response * pilot) / scale
  val h = (sReg zip pReg(kCnt)).map{ case (a, b) => Mux(b, a, -a) }
  val hH = h.map{_.conj()*scale}

  // STATE MACHINE
  val sInit = 0.U(2.W)
  val sWork = 1.U(2.W)
  val sDone = 2.U(2.W)
  val sHold = 3.U(2.W)
  val state = RegInit(sInit)

  // TODO: Is this correct?
  // Input & pilot ready only when in init or payload
  io.in.ready := state === sInit || state === sHold
  io.pilots.ready := state === sInit || state === sHold
  // External weight overriding can only be done in the payload phase
  io.extWt.ready := state === sHold
  // Output valid
  // TODO: Why does this complain?
  io.out.zipWithIndex.map{case(a,b) => a.valid := outValidReg(b)}

  // Load known pilots
  when(io.pilots.fire()) { pReg := io.pilots.bits }

  // When sInit load spectrum
  when(io.in.fire()) {
    sReg := io.in.bits
    state := sWork
  }

  // Calculate Hermitian conjugate
  when(state === sWork) {
    // TODO: Need to fix scale to make this not complain?
    wMem(kCnt) := hH
    state := sDone
  }

  // Set weights for user kCnt
  (io.out zip wMem).foreach{case(a,b) => a.bits := b}

  // this user's now valid, increment, move onto payload when finished
  when(state === sDone) {
    outValidReg(kCnt) := true.B
    kCnt := kCnt + 1.U
    when(kCnt === (params.K-1).U) { state := sHold }
    .otherwise { state := sInit }
  }

  // Wait for payload frames to finish. Each FFT valid is a new payload frame.
  when(state === sHold) {
    when(io.in.valid) { fCnt := fCnt + 1.U }
    when(fCnt === (params.F-1).U) {
      outValidReg.foreach(_ := false.B)
      state := sInit
      kCnt := 0.U
      fCnt := 0.U
    } .elsewhen(io.extWt.fire()) { // replace with external weights during payload frames if commanded
      wMem(io.extWt.bits.kAddr)(io.extWt.bits.sAddr) := io.extWt.bits.wt
    } .otherwise {
      state := sHold
    }
  }
}

/**
  * Mixin for top-level rocket to add a PWM
  *
  */

trait HasPeripheryFFTFSM extends BaseSubsystem {
  // instantiate FFTFSM chain
  val FFTFSMChain = LazyModule(new FFTFSMThing(FixedFFTFSMParams(IOWidth = 16, S = 256, K = 2, M = 4, F = 33, O = 1)))
  // connect memory interfaces to pbus
  pbus.toVariableWidthSlave(Some("FFTFSMWrite")) { FFTFSMChain.writeQueue.mem.get }
  pbus.toVariableWidthSlave(Some("FFTFSMRead")) { FFTFSMChain.readQueue.mem.get }
}
