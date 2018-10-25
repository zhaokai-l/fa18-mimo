package mimo

import chisel3._
import chisel3.experimental.FixedPoint
import chisel3.util.Decoupled
import chisel3.util.log2Ceil
import dsptools.numbers._
import dsptools.numbers.implicits._
import freechips.rocketchip.diplomacy.LazyModule
import freechips.rocketchip.subsystem.BaseSubsystem
import breeze.math.Complex

/**
  * In the pilot phase, the GolayFSM takes a peak from the Golay correlator (eventually this will be 5 samples around the peak).
  * There are K users in the system, and they will send staggered Golay code pilots of length N.
  * Therefore, each FSM is a single dimensional system. and there are M FSMs (M antennas).
  * The FSM uses finds the Hermitian conjugate of the correlation peak.
  * This is the weights for the M x K matrix multiplier.
  * Finally, in the payload phase, the M x K weights times the M received time domain signals gives the symbols for each of the K users.  * The FSM can replace any weight in the system from an external memory mapped interface during the payload phase.
  */

/**
  * Base class for GolayFSM parameters
  *
  * These are type generic
  */
trait GolayFSMParams[T <: Data] {
  // DspComplex
  val proto: DspComplex[T]
  // Length of Golay codeword
  val N: Int
  // Number of correlation samples around peak
  val C: Int
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
  * GolayFSM parameters object for fixed-point GolayFSMs
  */
case class FixedGolayFSMParams(
  // width of input spectrum & output weights, DspReal()
  IOWidth: Int,
  N: Int,
  C: Int,
  K: Int,
  M: Int,
  F: Int,
  O: Int,
) extends GolayFSMParams[FixedPoint] {
  // 1 sign & 1 integer bit since range is -1 to +1
  val proto = DspComplex(FixedPoint(IOWidth.W, (IOWidth-2).BP), FixedPoint(IOWidth.W, (IOWidth-2).BP))
}

/**
  * To override weight from AXI4. Valid pulse implies weight needs to be overridden.
  */
class GolayExtWtBundle[T <: Data](params: GolayFSMParams[T]) extends Bundle {
  val kAddr = UInt(log2Ceil(params.K).W)
  val cAddr = UInt(log2Ceil(params.C).W)
  val wt = params.proto.cloneType

  override def cloneType: this.type = GolayExtWtBundle(params).asInstanceOf[this.type]
}
object GolayExtWtBundle {
  def apply[T <: Data](params: GolayFSMParams[T]): GolayExtWtBundle[T] = new GolayExtWtBundle(params)
}

/**
  * Bundle type as IO for GolayFSM modules
  */
class GolayFSMIO[T <: Data](params: GolayFSMParams[T]) extends Bundle {
  // Correlation output
  val in = Flipped(Decoupled(Vec(params.C, params.proto.cloneType)))
  // Matrix weights
  val out = Vec(params.K, Decoupled(params.proto.cloneType))

  // Set weights externally
  val extWt = Flipped(Decoupled(new GolayExtWtBundle(params)))

  override def cloneType: this.type = GolayFSMIO(params).asInstanceOf[this.type]
}
object GolayFSMIO {
  def apply[T <: Data](params: GolayFSMParams[T]): GolayFSMIO[T] =
    new GolayFSMIO(params)
}

class GolayFSM[T <: Data : Real](val params: GolayFSMParams[T]) extends Module {
  // check parameters
  require(params.N > 0)
  require(params.C > 0)
  require(params.K > 0)
  require(params.M > 0)
  require(params.F > 0)
  require(params.O > 0)

  val io = IO(GolayFSMIO(params))

  // SETUP - regs

  // correlation
  val cReg = Reg(Vec(params.C, params.proto.cloneType))
  // weights
  val wMem = Reg(Vec(params.K, params.proto.cloneType))

  // pilot counter
  val kCnt = RegInit(0.U(log2Ceil(params.K).W))
  // payload counter
  val fCnt = RegInit(0.U(log2Ceil(params.F).W))

  // output valids
  val outValidReg = RegInit(Vec(Seq.fill(params.K)(false.B)))


  // COMBINATORIAL CALCULATION

  // scale down by # of antennas & oversampling ratio so that matrix mult sum = 1
  val scale = DspComplex(ConvertableTo[T].fromDouble(1.0/(params.M*params.O)), ConvertableTo[T].fromDouble(0))
  // TODO: ALL TIMING CORRECTION. This assumes only 1 correlation pt and it is the peak.
  // channel estimate = conjugate(correlation) / scale
  val hH = cReg(0).conj()*scale


  // STATE MACHINE
  val sInit = 0.U(2.W)
  val sWork = 1.U(2.W)
  val sDone = 2.U(2.W)
  val sHold = 3.U(2.W)
  val state = RegInit(sInit)

  // Input ready only when in init
  io.in.ready := state === sInit
  // External weight overriding can only be done in the payload phase
  io.extWt.ready := state === sHold
  // Output valid
  io.out.zipWithIndex.map{case(a,b) => a.valid := outValidReg(b)}

  // When sInit load spectrum
  when(io.in.fire()) {
    cReg := io.in.bits
    state := sWork
  }

  // Calculate Hermitian conjugate
  when(state === sWork) {
    wMem(kCnt) := hH
    state := sDone
  }

  // Set weights for user kCnt
  (io.out zip wMem).foreach{case(a,b) => a.bits := b}
  
  // this user now valid, increment, move onto payload when finished
  when(state === sDone) {
    outValidReg(kCnt) := true.B
    kCnt := kCnt + 1.U
    when(kCnt === (params.K-1).U) { state := sHold }
    .otherwise { state := sInit }
  }

  // Wait for payload frames to finish. Each correlation is a new payload frame.
  when(state === sHold) {
    when(io.in.valid) { fCnt := fCnt + 1.U }
    when(fCnt === (params.F-1).U) {
      outValidReg.foreach(_ := false.B)
      state := sInit
      kCnt := 0.U
      fCnt := 0.U
    } .elsewhen(io.extWt.fire()) {
      wMem(io.extWt.bits.kAddr) := io.extWt.bits.wt
    } .otherwise {
      state := sHold
    }
  }
}

/**
  * Mixin for top-level rocket to add a PWM
  *
  */

trait HasPeripheryGolayFSM extends BaseSubsystem {
  // instantiate GolayFSM chain
  val GolayFSMChain = LazyModule(new GolayFSMThing(FixedGolayFSMParams(IOWidth = 16, N = 32, C = 1, K = 2, M = 4, F = 33, O = 1)))
  // connect memory interfaces to pbus
  pbus.toVariableWidthSlave(Some("GolayFSMWrite")) { GolayFSMChain.writeQueue.mem.get }
  pbus.toVariableWidthSlave(Some("GolayFSMRead")) { GolayFSMChain.readQueue.mem.get }
}
