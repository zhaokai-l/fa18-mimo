package mimo

import chisel3._
import chisel3.experimental.FixedPoint
import chisel3.util.Decoupled
import chisel3.util._
import dsptools.numbers._
import freechips.rocketchip.diplomacy.LazyModule
import freechips.rocketchip.subsystem.BaseSubsystem


/**
  * Base class for SystolicArray parameters
  *
  * These are type generic
  */
trait SystolicArrayParams[T <: Data] {
  val inA: T
  val inB: T
  val outC: T

  // Multiply KxM matrix by a MxN matrix
  val K: Int
  val M: Int
  val N: Int

  val PEParam: PEParams[T]
}

/**
  * Bundle type that describes the input of SystolicArray
  */
class SystolicArrayInBundle[T <: Data](params: SystolicArrayParams[T]) extends Bundle {
  val a: T = params.inA.cloneType
  val b: T = params.inB.cloneType

  override def cloneType: this.type = SystolicArrayInBundle(params).asInstanceOf[this.type]
}
object SystolicArrayInBundle {
  def apply[T <: Data](params: SystolicArrayParams[T]): SystolicArrayInBundle[T] = new SystolicArrayInBundle(params)
}

/**
  * Bundle type that describes the output of SystolicArray
  */
class SystolicArrayOutBundle[T <: Data](params: SystolicArrayParams[T]) extends Bundle {
  val c: T = params.outC.cloneType

  override def cloneType: this.type = SystolicArrayOutBundle(params).asInstanceOf[this.type]
}
object SystolicArrayOutBundle {
  def apply[T <: Data](params: SystolicArrayParams[T]): SystolicArrayOutBundle[T] = new SystolicArrayOutBundle(params)
}



/**
  * Bundle type as IO for SystolicArray modules
  */
class SystolicArrayIO[T <: Data](params: SystolicArrayParams[T]) extends Bundle {
  val in = Flipped(Decoupled(SystolicArrayInBundle(params)))
  val out = Decoupled(SystolicArrayOutBundle(params))

  override def cloneType: this.type = SystolicArrayIO(params).asInstanceOf[this.type]
}
object SystolicArrayIO {
  def apply[T <: Data](params: SystolicArrayParams[T]): SystolicArrayIO[T] =
    new SystolicArrayIO(params)
}



/**
  * Base processing element (PE) for systolic array implementation of matrix multiplication
  */
class SystolicArray[T <: Data : Real](params: SystolicArrayParams[T]) extends Module {
  val io = IO(SystolicArrayIO(params))

//  val array = for (i <- 0 until params.K) yield {
//    for (j <- 0 until params.M) yield {
//      val PE = Module(new PE(params.PEParam))
//      PE
//    }
//  }

  // Get an array of PEs
  val array = Seq.fill(params.K)( Seq.fill(params.N)(Module(new PE(params.PEParam))) )
  // Connect the PEs
  for (i <- 0 until params.K-1) {
    for (j <- 0 until params.M-1) {
      array(i)(j+1).io.in.bits.b := array(i)(j).io.out.bits.b
      array(i+1)(j).io.in.bits.a := array(i)(j).io.out.bits.a
    }
  }


  // FSM states
  val INIT = 0.U(2.W)
  val WORK = 1.U(2.W)
  val DONE = 2.U(2.W)
  val state = RegInit(INIT)

  io.in.ready := (state === INIT | state === WORK)
  io.out.valid := false.B
  io.finalOut.valid := state === DONE

  // Iterations for working state
  val iter = RegInit(0.U(log2Ceil(params.nIters+1).W))
  val c = Reg(PEFinalOutBundle(params).c)


  when (state === INIT && io.in.fire()) {
    state := WORK
    c := io.in.bits.a * io.in.bits.b
    io.out.valid := true.B
    iter := iter + 1.U
  }
  when (state === WORK && io.in.fire())  {
    c := io.in.bits.a * io.in.bits.b + c
    io.out.valid := true.B
    iter := iter + 1.U
    when (iter >= (params.nIters-1).U) {
      state := DONE
    }
  }
  when (state === DONE && io.finalOut.fire()) {
    iter := 0.U
    state := INIT
  }

  io.out.bits.a := io.in.bits.a
  io.out.bits.b := io.in.bits.b
  io.finalOut.bits.c := c
}