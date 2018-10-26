package mimo

import chisel3._
import chisel3.experimental.FixedPoint
import chisel3.util.Decoupled
import chisel3.util._
import dsptools.numbers._
import freechips.rocketchip.diplomacy.LazyModule
import freechips.rocketchip.subsystem.BaseSubsystem


/**
  * Base class for PE parameters
  *
  * These are type generic
  */
trait PEParams[T <: Data] {
  val inA: T
  val inB: T
  val outA: T
  val outB: T
  val outC: T
  val K: Int
  val M: Int
  val N: Int
}

/**
  * Bundle type that describes the input of PE
  */
class PEInBundle[T <: Data](params: PEParams[T]) extends Bundle {
  val a = Vec(params.K, params.inA.cloneType)
  val b = Vec(params.N, params.inB.cloneType)

  override def cloneType: this.type = PEInBundle(params).asInstanceOf[this.type]
}
object PEInBundle {
  def apply[T <: Data](params: PEParams[T]): PEInBundle[T] = new PEInBundle(params)
}

/**
  * Bundle type that describes the output of PE
  */
class PEOutBundle[T <: Data](params: PEParams[T]) extends Bundle {
  val a = Vec(params.K, params.outA.cloneType)
  val b = Vec(params.N, params.outB.cloneType)

  override def cloneType: this.type = PEOutBundle(params).asInstanceOf[this.type]
}
object PEOutBundle {
  def apply[T <: Data](params: PEParams[T]): PEOutBundle[T] = new PEOutBundle(params)
}

/**
  * Bundle type that describes the output of PE
  */
class PEFinalOutBundle[T <: Data](params: PEParams[T]) extends Bundle {
  val c = Vec(params.K, Vec(params.N, params.outC.cloneType))

  override def cloneType: this.type = PEFinalOutBundle(params).asInstanceOf[this.type]
}
object PEFinalOutBundle {
  def apply[T <: Data](params: PEParams[T]): PEFinalOutBundle[T] = new PEFinalOutBundle(params)
}

/**
  * Bundle type as IO for PE modules
  */
class PEIO[T <: Data](params: PEParams[T]) extends Bundle {
  val in = Flipped(Decoupled(PEInBundle(params)))
  val out = Decoupled(PEOutBundle(params))
  val finalOut = Decoupled(PEFinalOutBundle(params))

  override def cloneType: this.type = PEIO(params).asInstanceOf[this.type]
}
object PEIO {
  def apply[T <: Data](params: PEParams[T]): PEIO[T] =
    new PEIO(params)
}



/**
  * Base processing element (PE) for systolic array implementation of matrix multiplication
  */
class PE[T <: Data : Ring](params: PEParams[T]) extends Module {
  val io = IO(PEIO(params))

  // FSM states
  val INIT = 0.U(2.W)
  val WORK = 1.U(2.W)
  val DONE = 2.U(2.W)
  val state = RegInit(INIT)

  io.in.ready := (state === INIT | state === WORK)
  io.out.valid := false.B
  io.finalOut.valid := state === DONE

  // Iterations for working state
  val iter = RegInit(0.U(log2Ceil(params.M+1).W))
  val c = Seq.fill(params.K)(Seq.fill(params.N)(Reg(params.outC.cloneType)))


  when (state === INIT && io.in.fire()) {
    state := WORK
    c.zipWithIndex.map { case(cVec, indK) => cVec.zipWithIndex.map { case(cVal, indN) => cVal := io.in.bits.a(indK) * io.in.bits.b(indN) } }
//    for (k <- 0 until params.K) {
//      for (n <- 0 until params.N) {
//        c()
//      }
//    }
    io.out.valid := true.B
    iter := iter + 1.U
  }
  when (state === WORK && io.in.fire())  {
    c.zipWithIndex.map { case(cVec, indK) => cVec.zipWithIndex.map { case(cVal, indN) => cVal := io.in.bits.a(indK) * io.in.bits.b(indN) + cVal } }
    io.out.valid := true.B
    iter := iter + 1.U
    when (iter >= (params.M-1).U) {
      state := DONE
    }
  }
  when (state === DONE && io.finalOut.fire()) {
    iter := 0.U
    state := INIT
  }

  io.out.bits.a := io.in.bits.a
  io.out.bits.b := io.in.bits.b
  io.finalOut.bits.c.zipWithIndex.map { case(cVec, indK) => cVec.zipWithIndex.map { case(cVal, indN) => cVal := c(indK)(indN) } }
  //io.finalOut.bits.c := c
}