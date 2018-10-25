package mimo

import dsptools.DspTester
import chisel3.experimental.FixedPoint
import dsptools.numbers.DspComplex
import breeze.math.Complex

/**
  * Case class holding information needed to run an individual test
  */
case class ABC(
                // input
                ain: Seq[Seq[Double]],
                bin: Seq[Seq[Double]],
                // optional outputs
                // if None, then don't check the result
                // if Some(...), check that the result matches
                cout: Option[Seq[Seq[Double]]] = None
              )

case class ABCComplex(
                // input
                ain: Seq[Seq[Complex]],
                bin: Seq[Seq[Complex]],
                // optional outputs
                // if None, then don't check the result
                // if Some(...), check that the result matches
                cout: Option[Seq[Seq[Complex]]] = None
              )

/**
  * DspTester for PE
  *
  * Run each trial in @trials
  */
class PETester[T <: chisel3.Data](c: PE[T], trials: Seq[ABC], tolLSBs: Int = 3) extends DspTester(c) {
  val maxCyclesWait = 50

  poke(c.io.finalOut.ready, 1)
  poke(c.io.in.valid, 1)

  for (trial <- trials) {
    for (in <- trial.ain zip trial.bin) {

      // wait until input is accepted
      var cyclesWaiting = 0
      while (!peek(c.io.in.ready) && cyclesWaiting < maxCyclesWait) {
        cyclesWaiting += 1
        if (cyclesWaiting >= maxCyclesWait) {
          expect(false, "waited for input too long")
        }
        step(1)
      }

      c.io.in.bits.a.zipWithIndex.map{ case(ain,ind) => poke(ain, in._1(ind)) }
      c.io.in.bits.b.zipWithIndex.map{ case(bin,ind) => poke(bin, in._2(ind)) }
      step(1)
    }
    // wait until output is valid
    var cyclesWaiting = 0
    while (!peek(c.io.finalOut.valid) && cyclesWaiting < maxCyclesWait) {
      cyclesWaiting += 1
      if (cyclesWaiting >= maxCyclesWait) {
        expect(false, "waited for output too long")
      }
      step(1)
    }

    // set desired tolerance
    // in this case, it's pretty loose (2 bits)
    // can you get tolerance of 1 bit? 0? what makes the most sense?
    fixTolLSBs.withValue(tolLSBs) {
      // check every output where we have an expected value
      //trial.cout.foreach { x => expect(c.io.finalOut.bits.c, x) }
      trial.cout.foreach { xMat => xMat.zipWithIndex.map{ case(xVec, indK) => xVec.zipWithIndex.map { case(x, indN) => expect(c.io.finalOut.bits.c(indK)(indN), x) } } }
    }
  }
}

/**
  * DspTester for PE
  *
  * Run each trial in @trials
  */
class ComplexPETester[T <: chisel3.Data](c: PE[T], trials: Seq[ABCComplex], tolLSBs: Int = 2) extends DspTester(c) {
  val maxCyclesWait = 50

  poke(c.io.finalOut.ready, 1)
  poke(c.io.in.valid, 1)

  for (trial <- trials) {
    for (in <- trial.ain zip trial.bin) {

      // wait until input is accepted
      var cyclesWaiting = 0
      while (!peek(c.io.in.ready) && cyclesWaiting < maxCyclesWait) {
        cyclesWaiting += 1
        if (cyclesWaiting >= maxCyclesWait) {
          expect(false, "waited for input too long")
        }
        step(1)
      }

//      c.io.in.bits.a.zipWithIndex.map{ case(ain,ind) => poke(ain, in._1(ind)) }
//      c.io.in.bits.b.zipWithIndex.map{ case(bin,ind) => poke(bin, in._2(ind)) }
      step(1)
    }
    // wait until output is valid
    var cyclesWaiting = 0
    while (!peek(c.io.finalOut.valid) && cyclesWaiting < maxCyclesWait) {
      cyclesWaiting += 1
      if (cyclesWaiting >= maxCyclesWait) {
        expect(false, "waited for output too long")
      }
      step(1)
    }

    // set desired tolerance
    // in this case, it's pretty loose (2 bits)
    // can you get tolerance of 1 bit? 0? what makes the most sense?
    fixTolLSBs.withValue(tolLSBs) {
      // check every output where we have an expected value
      //trial.cout.foreach { xMat => xMat.zipWithIndex.map{ case(xVec, indK) => xVec.zipWithIndex.map { case(x, indN) => expect(c.io.finalOut.bits.c(indK)(indN), x) } } }
    }
  }
}

/**
  * Convenience function for running tests
  */
object FixedPETester {
  def apply(params: PEParams[FixedPoint], trials: Seq[ABC]): Boolean = {
    chisel3.iotesters.Driver.execute(Array("-tbn", "firrtl", "-fiwv"), () => new PE(params)) {
      c => new PETester(c, trials = trials)
    }
  }
}

object RealPETester {
  def apply(params: PEParams[dsptools.numbers.DspReal], trials: Seq[ABC]): Boolean = {
    chisel3.iotesters.Driver.execute(Array("-tbn", "verilator", "-fiwv"), () => new PE(params)) {
      c => new PETester(c, trials = trials)
    }
  }
}

object FixedComplexPETester {
  def apply(params: PEParams[DspComplex[FixedPoint]], trials: Seq[ABCComplex]): Boolean = {
    chisel3.iotesters.Driver.execute(Array("-tbn", "firrtl", "-fiwv"), () => new PE(params)) {
      c => new ComplexPETester(c, trials)
    }
  }
}
