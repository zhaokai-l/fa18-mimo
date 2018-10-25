package mimo

import chisel3.experimental.FixedPoint
import dsptools.numbers._
import org.scalatest.{FlatSpec, Matchers}
import chisel3._
import chisel3.experimental.FixedPoint
import chisel3.util.Decoupled
import chisel3.util._
import dsptools.numbers._
import freechips.rocketchip.diplomacy.LazyModule
import freechips.rocketchip.subsystem.BaseSubsystem

import scala.util.Random

class PESpec extends FlatSpec with Matchers {
  behavior of "PE"

  val realParams = new PEParams[DspReal] {
    val inA = DspReal()
    val inB = DspReal()
    val outA = DspReal()
    val outB = DspReal()
    val outC = DspReal()
    val nIters = 3
  }

  val IOWidth = 10
  val fixedParams = new PEParams[FixedPoint] {
    val inA = FixedPoint(IOWidth.W,(IOWidth-2).BP)
    val inB = FixedPoint(IOWidth.W,(IOWidth-2).BP)
    val outA = FixedPoint(IOWidth.W,(IOWidth-2).BP)
    val outB = FixedPoint(IOWidth.W,(IOWidth-2).BP)
    val outC = FixedPoint(IOWidth.W,(IOWidth-2).BP)
    val nIters = 3
  }

  it should "real multiply" in {
    val numTrials = 10

    val trials = Seq.fill(numTrials) {
      val a = Seq.fill(realParams.nIters)(Random.nextDouble())
      val b = Seq.fill(realParams.nIters)(Random.nextDouble())
      val c = (a zip b).map { case (a: Double, b: Double) => a * b }.sum
      ABC(ain=a, bin=b, cout=Some(c))
    }

    RealPETester(realParams, trials) should be (true)
  }

  it should "fixed multiply" in {
    val numTrials = 10

    val trials = Seq.fill(numTrials) {
      val a = Seq.fill(fixedParams.nIters)(Random.nextDouble())
      val b = Seq.fill(fixedParams.nIters)(Random.nextDouble())
      val c = (a zip b).map { case (a: Double, b: Double) => a * b }.sum
      ABC(ain=a, bin=b, cout=Some(c))
    }

    FixedPETester(fixedParams, trials) should be (true)
  }

}
