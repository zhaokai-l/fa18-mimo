package mimo

import dsptools.DspTester
import breeze.math.Complex

/**
 * Case class holding information needed to run an individual test
 */
case class PSW(
  // Known BPSK pilots
  pilot: Seq[Boolean],
  // Received spectrum
  spectrum: Seq[Complex],
  // optional outputs
  // if None, then don't check the result
  // if Some(...), check that the result matches
  weights: Option[Seq[Complex]] = None
)

/**
 * DspTester for FFTFSM
 *
 * Run each trial in @trials
 */
class FFTFSMTester[T <: chisel3.Data](c: FFTFSM[T], frames: Seq[PSW], tolLSBs: Int = 2) extends DspTester(c) {
  val maxCyclesWait = 50

  // Set the input valid (all subcarriers
  poke(c.io.in.valid, 1)
  poke(c.io.pilots.valid, 1)

  // TODO: external weight setting
  poke(c.io.extWt.valid, 0)
  poke(c.io.extWt.bits.kAddr, 0)
  poke(c.io.extWt.bits.sAddr, 0)
  poke(c.io.extWt.bits.wt, Complex(0, 0))

  // output ready for each user
  c.io.out.foreach{ k => poke(k.ready, 1) }

  var i = 0

  // Iterate through all frames
  for ((frame, k) <- frames.zipWithIndex) {
    // load known pilots and received spectrums
    // (one-by-one as DspTester doesn't support poking Seqs of Complex)
    // need to keep track of when pilots are sent
    i = k %( c.params.K+c.params.F)
    if (i < c.params.K) {
      (c.io.pilots.bits(i) zip frame.pilot).foreach{ case(a,b) => poke(a,b) }
    }
    (c.io.in.bits zip frame.spectrum).foreach{ case(a,b) => poke(a,b) }

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
    else { step(1) }
    // set desired tolerance
    // in this case, it's pretty loose (2 bits)
    // can you get tolerance of 1 bit? 0? what makes the most sense?
    fixTolLSBs.withValue(tolLSBs) {
      // check every output where we have an expected value
      // TODO: Support case where we want to see that the weights didn't change during payload
      frame.weights.foreach {
        w => (c.io.out(k).bits zip w).foreach { case(a,b) => expect(a,b) }
      }
    }
  }
}

/**
 * Convenience function for running tests
 */
object FixedFFTFSMTester {
  def apply(params: FixedFFTFSMParams, frames: Seq[PSW]): Boolean = {
    chisel3.iotesters.Driver.execute(Array("-tbn", "firrtl", "-fiwv"), () => new FFTFSM(params)) {
      c => new FFTFSMTester(c, frames)
    }
  }
}

object RealFFTFSMTester {
  def apply(params: FFTFSMParams[dsptools.numbers.DspReal], frames: Seq[PSW]): Boolean = {
    chisel3.iotesters.Driver.execute(Array("-tbn", "verilator", "-fiwv"), () => new FFTFSM(params)) {
      c => new FFTFSMTester(c, frames)
    }
  }
}
