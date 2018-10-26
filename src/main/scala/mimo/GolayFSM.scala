package mimo

import chisel3._
import chisel3.experimental.FixedPoint
import chisel3.util.Decoupled
import chisel3.util.log2Ceil
import dsptools.numbers._
import dsptools.numbers.implicits._
import freechips.rocketchip.diplomacy.LazyModule
import freechips.rocketchip.subsystem.BaseSubsystem

/**
  * The GolayFSM is much simpler than the FFTFSM.
  * It takes a streaming correlation. The correlator asserts valid when a peak is found.
  * Each peak corresponds to a user's pilot -> channel estimation for each user.
  * The FSM can replace any weight in the system from an external memory mapped interface during the payload phase.
  */

/**
  * Base class for GolayFSM parameters
  *
  * These are type generic
  */
trait GolayFSMParams[T <: Data] {
  // DspComplex
  val proto: DspComplex[T]
  // Number of correlation samples around peak
  val C: Int
  // Number of UEs (pilot frames)
  val K: Int
  // Oversampling ratio
  val O: Int
}

/**
  * GolayFSM parameters object for fixed-point GolayFSMs
  */
case class FixedGolayFSMParams(
  // width of input spectrum & output weights, DspReal()
  IOWidth: Int,
  C: Int,
  K: Int,
  O: Int,
) extends GolayFSMParams[FixedPoint] {
  // TODO: range can be fixed because input power normalized to 1?
  val proto = DspComplex(FixedPoint(IOWidth.W, (IOWidth-2).BP), FixedPoint(IOWidth.W, (IOWidth-2).BP))
}

/**
  * To override weight from AXI4. Valid pulse implies weight needs to be overridden.
  */
class GolayExtWtBundle[T <: Data](params: GolayFSMParams[T]) extends Bundle {
  val kAddr = UInt(log2Ceil(params.K).W)
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
  require(params.C > 0)
  require(params.K > 0)
  require(params.O > 0)

  val io = IO(GolayFSMIO(params))

  // SETUP - regs

  // correlation
  val cReg = Reg(Vec(params.C, params.proto.cloneType))
  // weights
  val wMem = Reg(Vec(params.K, params.proto.cloneType))
  // pilot counter
  val kCnt = RegInit(0.U(log2Ceil(params.K).W))
  // output valids
  val outValidReg = RegInit(Vec(Seq.fill(params.K)(false.B)))


  // COMBINATORIAL CALCULATION

  // TODO: ALL TIMING CORRECTION. This assumes only 1 correlation pt and it is the peak.
  // channel estimate = conjugate(correlation)
  val hH = cReg(0).conj()


  // STATE MACHINE
  val sInit = 0.U(2.W)
  val sWork = 1.U(2.W)
  val sDone = 2.U(2.W)
  val state = RegInit(sInit)

  // Input always ready except when calculating weight
  io.in.ready := state === sInit
  // External weight overriding can only be done when not calculating
  io.extWt.ready := state === sInit
  // Output valid
  io.out.zipWithIndex.map{case(o,i) => o.valid := outValidReg(i)}

  // When sInit
  // reset counter if new pilots, load correlation peak
  // else grab external weight if available
  when(state === sInit) {
    when(io.in.fire()) {
      when(kCnt === (params.K-1).U) {
        kCnt := 0.U
        outValidReg.foreach{_ := false.B}
      }
      cReg := io.in.bits
      state := sWork
    } .elsewhen(io.extWt.fire()) {
      wMem(io.extWt.bits.kAddr) := io.extWt.bits.wt
      state := sInit
    } .otherwise {
      state := sInit
    }
  }

  // Calculate Hermitian conjugate
  when(state === sWork) {
    wMem(kCnt) := hH
    state := sDone
  }

  // Set weights for user kCnt
  (io.out zip wMem).foreach{case(o,w) => o.bits := w}
  
  // this user now valid, increment user counter, wait for next peak
  when(state === sDone) {
    outValidReg(kCnt) := true.B
    kCnt := kCnt + 1.U
    state := sInit
  }
}

/**
  * Mixin for top-level rocket to add a PWM
  *
  */

trait HasPeripheryGolayFSM extends BaseSubsystem {
  // instantiate GolayFSM chain
  val GolayFSMChain = LazyModule(new GolayFSMThing(FixedGolayFSMParams(IOWidth = 16, C = 1, K = 2, O = 1)))
  // connect memory interfaces to pbus
  pbus.toVariableWidthSlave(Some("GolayFSMWrite")) { GolayFSMChain.writeQueue.mem.get }
  pbus.toVariableWidthSlave(Some("GolayFSMRead")) { GolayFSMChain.readQueue.mem.get }
}
