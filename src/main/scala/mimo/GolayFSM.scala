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
  // width of input spectrum & output weights
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
  * Bundle type as IO for GolayFSM modules
  */
class GolayFSMIO[T <: Data](params: GolayFSMParams[T]) extends Bundle {
  // Correlation output
  val in = Flipped(Decoupled(Vec(params.C, params.proto.cloneType)))
  // Matrix weights
  val out = Vec(params.K, Decoupled(params.proto.cloneType))

  // Inputs to set weights externally
  val ext = Input(Bool())
  val kAddr = Input(log2Ceil(params.K).U) // Which UE
  val extWt = Flipped(Decoupled(params.proto.cloneType)) // The weight

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

  // Register to store the correlation
  val sReg = Reg(Vec(params.C, params.proto.cloneType))

  // Memory to store the weights
  val wMem = Reg(Vec(params.K, params.proto.cloneType))

  // Counter for which UE is sending pilot
  val kCnt = RegInit(0.U(log2Ceil(params.K).W))
  // Counter for the frame (to know when to readjust the weights)
  val fCnt = RegInit(0.U(log2Ceil(params.F).W))

  // STATE MACHINE
  val sInit = 0.U(2.W)
  val sWork = 1.U(2.W)
  val sDone = 2.U(2.W)
  val sHold = 3.U(2.W)
  val state = RegInit(sInit)

  // Input ready only when in init
  io.in.ready := state === sInit

  // When init load spectrum
  when(io.in.fire()) {
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
  //(io.out zip wMem).foreach{case(a,b) => a.bits := b}
  for(k <- 0 until params.K) {
    io.out(k).bits := wMem(k)
  }
  when(state === sDone && io.out(kCnt).fire()) {
    // next user's weights are now valid
    io.out(kCnt).valid := true.B
    // increment user
    kCnt := kCnt + 1.U
    // when all users calculated, move onto payload
    when(kCnt === params.K.U) {
      state := sHold
    } .otherwise {
      state := sInit
    }
  }

  // Wait for payload frames to finish
  when(state === sHold) {
    // each FFT valid is a new payload frame
    when(io.in.valid) {
      fCnt := fCnt + 1.U
    }
    // replace with external weights during payload frames if commanded
    // when payload frames are over, return back to sInit
    when(fCnt === params.F.U) {
      // reset all output weights
      for(k <- 0 until params.K) {
        io.out(k).valid := false.B
      }
      state := sInit
      kCnt := 0.U
      fCnt := 0.U
    } .elsewhen(io.ext && io.extWt.fire()) {
      wMem(io.kAddr) := io.extWt.bits
    }
  }

  // COMBINATORIAL CALCULATION

  // need to scale down by # of antennas so that matrix mult sum = 1
  // TODO: this isn't type generic. Somehow can't get it to convert to params.proto.
  val scale = DspComplex(ConvertableTo[T].fromDouble(1/(params.M*params.N*params.O)), ConvertableTo[T].fromDouble(0))
  // this doesn't work
  // val scale = params.proto(Complex(1/params.M, 0))

  // calculate the complex conjugate and then scale down
  val hH = VecInit(sReg.map{_.conj() * scale})

}

/**
  * Mixin for top-level rocket to add a PWM
  *
  */
/*
trait HasPeripheryGolayFSM extends BaseSubsystem {
  // instantiate GolayFSM chain
  val GolayFSMChain = LazyModule(new GolayFSMThing(FixedGolayFSMParams(8, 10)))
  // connect memory interfaces to pbus
  pbus.toVariableWidthSlave(Some("GolayFSMWrite")) { GolayFSMChain.writeQueue.mem.get }
  pbus.toVariableWidthSlave(Some("GolayFSMRead")) { GolayFSMChain.readQueue.mem.get }
}
*/