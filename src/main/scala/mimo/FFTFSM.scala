package mimo

import chisel3._
import chisel3.experimental.FixedPoint
import chisel3.util.Decoupled
import chisel3.util.log2Ceil
import dsptools.numbers._
import freechips.rocketchip.diplomacy.LazyModule
import freechips.rocketchip.subsystem.BaseSubsystem

/**
 * Base class for FFTFSM parameters
 *
 * These are type generic
 */
trait FFTFSMParams[T <: Data] {
  // DspComplex
  val proto: T
  // Number of subcarriers
  val S: Int
  // Number of UEs (pilot frames)
  val K: Int
  // Number of antennas
  val M: Int
  // Number of payload frames
  val F: Int
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
) extends FFTFSMParams[DspComplex[FixedPoint]] {
  // 1 sign & 1 integer bit since range is -1 to +1
  val proto = DspComplex(FixedPoint(IOWidth.W, (IOWidth-2).BP), FixedPoint(IOWidth.W, (IOWidth-2).BP))
}

/**
 * Bundle type that describes the input, state, and output of FFTFSM
 */
/*
class FFTFSMBundle[T <: Data](params: FFTFSMParams[T]) extends Bundle {
  val slice = Vec(params.S, params.proto.cloneType)

  override def cloneType: this.type = FFTFSMBundle(params).asInstanceOf[this.type]
}
object FFTFSMBundle {
  def apply[T <: Data](params: FFTFSMParams[T]): FFTFSMBundle[T] = new FFTFSMBundle(params)
}*/

/**
 * Bundle type as IO for FFTFSM modules
 */
class FFTFSMIO[T <: Data](params: FFTFSMParams[T]) extends Bundle {
  // FFT spectrum
  val in = Flipped(Decoupled(Vec(params.S, params.proto.cloneType)))
  // Matrix weights
  val out = Vec(params.K, Decoupled(Vec(params.S, params.proto.cloneType)))

  // Inputs to set weights externally
  val ext = Input(Bool())
  val KAddr = Input(log2Ceil(params.K).U) // Which UE
  val SAddr = Input(log2Ceil(params.S).U) // Which subcarrier
  val extWt = Flipped(Decoupled(params.proto.cloneType)) // The weight

  override def cloneType: this.type = FFTFSMIO(params).asInstanceOf[this.type]
}
object FFTFSMIO {
  def apply[T <: Data](params: FFTFSMParams[T]): FFTFSMIO[T] =
    new FFTFSMIO(params)
}

object AddSub {
  def apply[T <: Data : Ring](sel: Bool, a: T, b: T): T = {
    Mux(sel, a + b, a - b)
  }
}

class FFTFSM[T <: Data](val params: FFTFSMParams[T]) extends Module {
  // check parameters
  require(params.S > 0)
  require(params.K > 0)

  val io = IO(FFTFSMIO(params))

  // SETUP - regs, wires, mems, constants

  // Register to store the spectrum
  val sReg = Reg(Vec(params.S, params.proto.cloneType))

  // ROM with pilot sequence for each user
  val pilots = VecInit()

  // Wire with calculated Hermitian conjugate of channel response
  val hH = Wire(Vec(params.S, params.proto.cloneType))

  // Memory to store the weights
  val wMem = Reg(Vec(params.K, Vec(params.S, params.proto.cloneType)))

  // Counter for which UE is sending pilot
  val kCnt = RegInit(0.U(log2Ceil(params.K).W))
  // Counter for the frame (to know when to readjust the weights)
  val fCnt = RegInit(0.U(log2Ceil(params.F).W))

  // STATE MACHINE
  val sInit = 0.U(2.W)
  val sWork = 1.U(2.W)
  val sDone = 2.U(2.W)
  val sWait = 3.U(2.W)
  val state = RegInit(sInit)

  // Input ready only when in init
  io.in.ready := state === sInit

  // When init load spectrum
  when(state === sInit && io.in.fire()) {
    sReg := io.in.bits
    state := sWork
  }

  // Calculate Hermitian conjugate
  when(state === sWork) {
    wMem(kCnt) := hH
    state := sDone
  }

  // Set weights for user kCnt
  // TODO: Figure out how to bulk assign Vec of Decoupled
  // io.out
  when(state === sDone && io.out(kCnt).fire()) {
    // next user's weights are now valid
    io.out(kCnt).valid := true.B
    // increment user
    kCnt := kCnt + 1
    // when all users calculated, move onto payload
    when(kCnt === params.K.U) {
      state := sWait
    } .otherwise {
      state := sInit
    }
  }

  // Wait for payload frames to finish
  when(state === sWait) {
    // each FFT ready is a new payload frame
    when(io.in.fire()) {
      fCnt := fCnt + 1
    }
    // replace with external weights
    when(fCnt === params.F.U) {
      // reset all output weights
      for(k <- 0 until params.K) {
        io.out(k).valid := false.B
      }
      state := sInit
      kCnt := 0
      fCnt := 0
    }
  }

  // COMBINATORIAL CALCULATION

}

/**
  * Mixin for top-level rocket to add a PWM
  *
  */
trait HasPeripheryFFTFSM extends BaseSubsystem {
  // instantiate FFTFSM chain
  val FFTFSMChain = LazyModule(new FFTFSMThing(FixedFFTFSMParams(8, 10)))
  // connect memory interfaces to pbus
  pbus.toVariableWidthSlave(Some("FFTFSMWrite")) { FFTFSMChain.writeQueue.mem.get }
  pbus.toVariableWidthSlave(Some("FFTFSMRead")) { FFTFSMChain.readQueue.mem.get }
}
