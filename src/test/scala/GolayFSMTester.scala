package mimo

import dsptools.DspTester
import breeze.math.Complex

/**
 * Case class holding information needed to run an individual test
 */
case class CW(
  // Received correlation
  correlation: Seq[Complex],
  // optional outputs
  // if None, then don't check the result
  // if Some(...), check that the result matches
  weight: Option[Complex] = None
)

/**
 * DspTester for GolayFSM
 *
 * Run each trial in @trials
 */
class GolayFSMTester[T <: chisel3.Data](c: GolayFSM[T], frames: Seq[CW], tolLSBs: Int = 2) extends DspTester(c) {
  val maxCyclesWait = 50

  // Set the input valid
  // TODO: is this streaming essentially?
  poke(c.io.in.valid, 1)

  // TODO: external weight setting
  poke(c.io.extWt.valid, 0)
  poke(c.io.extWt.bits.kAddr, 0)
  poke(c.io.extWt.bits.cAddr, 0)
  poke(c.io.extWt.bits.wt, Complex(0, 0))

  // output ready for each user
  c.io.out.foreach{ k => poke(k.ready, 1) }

  var i = 0

  // Iterate through all frames
  for ((frame, k) <- frames.zipWithIndex) {
    // load correlations
    // (one-by-one as DspTester doesn't support poking Seqs of Complex)
    // need to keep track of when pilots are sent
    i = k % (c.params.K+c.params.F)
    (c.io.in.bits zip frame.correlation).foreach{ case(a,b) => poke(a,b) }

    // wait until input is accepted
    var cyclesWaiting = 0
    while (!peek(c.io.in.ready) && cyclesWaiting < maxCyclesWait) {
      cyclesWaiting += 1
      if (cyclesWaiting >= maxCyclesWait) {
        expect(false, "waited for input too long")
      }
      step(1)
    }
    // wait until output is valid
    cyclesWaiting = 0
    // only wait for output valid in estimation phase
    if (i < c.params.K) {
      while (!peek(c.io.out(i).valid) && cyclesWaiting < maxCyclesWait) {
        cyclesWaiting += 1
        if (cyclesWaiting >= maxCyclesWait) {
          expect(false, "waited for output too long")
        }
        step(1)
      }
    }
    // else skip and finish all payload words
    else {
      step(1)
    }
    // set desired tolerance
    // in this case, it's pretty loose (2 bits)
    // can you get tolerance of 1 bit? 0? what makes the most sense?
    fixTolLSBs.withValue(tolLSBs) {
      // check every output where we have an expected value
      // TODO: Support case where we want to see that the weights didn't change during payload
      frame.weight.foreach {
        w => expect(c.io.out(i).bits, w)
      }
    }
  }
}

/**
 * Convenience function for running tests
 */
object FixedGolayFSMTester {
  def apply(params: FixedGolayFSMParams, frames: Seq[CW]): Boolean = {
    chisel3.iotesters.Driver.execute(Array("-tbn", "firrtl", "-fiwv"), () => new GolayFSM(params)) {
      c => new GolayFSMTester(c, frames)
    }
  }
}

object RealGolayFSMTester {
  def apply(params: GolayFSMParams[dsptools.numbers.DspReal], frames: Seq[CW]): Boolean = {
    chisel3.iotesters.Driver.execute(Array("-tbn", "verilator", "-fiwv"), () => new GolayFSM(params)) {
      c => new GolayFSMTester(c, frames)
    }
  }
}
