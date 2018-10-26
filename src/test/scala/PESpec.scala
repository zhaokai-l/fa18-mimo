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
import breeze.math.Complex

import scala.util.Random

class PESpec extends FlatSpec with Matchers {
  behavior of "PE"

  val realParams = new PEParams[DspReal] {
    val inA = DspReal()
    val inB = DspReal()
    val outA = DspReal()
    val outB = DspReal()
    val outC = DspReal()
    val K = 4
    val M = 3
    val N = 5
  }

  val IOWidth = 10
  val fixedParams = new PEParams[FixedPoint] {
    val inA = FixedPoint(IOWidth.W,(IOWidth-4).BP)
    val inB = FixedPoint(IOWidth.W,(IOWidth-4).BP)
    val outA = FixedPoint(IOWidth.W,(IOWidth-4).BP)
    val outB = FixedPoint(IOWidth.W,(IOWidth-4).BP)
    val outC = FixedPoint(IOWidth.W,(IOWidth-4).BP)
    val K = 4
    val M = 3
    val N = 5
  }

  val fixedComplexParams = new PEParams[DspComplex[FixedPoint]] {
    val inA = DspComplex(FixedPoint(IOWidth.W,(IOWidth-4).BP))
    val inB = DspComplex(FixedPoint(IOWidth.W,(IOWidth-4).BP))
    val outA = DspComplex(FixedPoint(IOWidth.W,(IOWidth-4).BP))
    val outB = DspComplex(FixedPoint(IOWidth.W,(IOWidth-4).BP))
    val outC = DspComplex(FixedPoint(IOWidth.W,(IOWidth-4).BP))
    val K = 4
    val M = 3
    val N = 1
  }

  it should "real multiply" in {
    val numTrials = 10

    val trials = Seq.fill(numTrials) {
      val a = Seq.fill(realParams.M)(Seq.fill(realParams.K)(Random.nextDouble()))
      val b = Seq.fill(realParams.M)(Seq.fill(realParams.N)(Random.nextDouble()))

      var c = Seq[Seq[Double]]()
      for (k <- 0 until realParams.K) {
        var crow = Seq[Double]()
        for (n <- 0 until realParams.N) {
          var sum = 0.0
          for (m <- 0 until realParams.M) {
            sum = sum + a(m)(k) * b(m)(n)
          }
          crow = crow :+ sum
        }
        c = c :+ crow
      }
      ABC(ain=a, bin=b, cout=Some(c))
    }

    RealPETester(realParams, trials) should be (true)
  }

  it should "fixed multiply" in {
    val numTrials = 10

    val trials = Seq.fill(numTrials) {
      val a = Seq.fill(fixedParams.M)(Seq.fill(fixedParams.K)(Random.nextDouble()))
      val b = Seq.fill(fixedParams.M)(Seq.fill(fixedParams.N)(Random.nextDouble()))

      var c = Seq[Seq[Double]]()
      for (k <- 0 until fixedParams.K) {
        var crow = Seq[Double]()
        for (n <- 0 until fixedParams.N) {
          var sum = 0.0
          for (m <- 0 until fixedParams.M) {
            sum = sum + a(m)(k) * b(m)(n)
          }
          crow = crow :+ sum
        }
        c = c :+ crow
      }
      ABC(ain=a, bin=b, cout=Some(c))
    }

    FixedPETester(fixedParams, trials) should be (true)
  }

  it should "fixed complex multiply" in {
    val numTrials = 10

    val trials = Seq.fill(numTrials) {
      val a = Seq.fill(fixedComplexParams.M)(Seq.fill(fixedComplexParams.K)(Complex(Random.nextDouble(), Random.nextDouble())))
      val b = Seq.fill(fixedComplexParams.M)(Seq.fill(fixedComplexParams.N)(Complex(Random.nextDouble(), Random.nextDouble())))

      var c = Seq[Seq[Complex]]()
      for (k <- 0 until fixedComplexParams.K) {
        var crow = Seq[Complex]()
        for (n <- 0 until fixedComplexParams.N) {
          var sum = Complex(0.0, 0.0)
          for (m <- 0 until fixedComplexParams.M) {
            sum = sum + a(m)(k) * b(m)(n)
          }
          crow = crow :+ sum
        }
        c = c :+ crow
      }
      ABCComplex(ain=a, bin=b, cout=Some(c))
    }

    FixedComplexPETester(fixedComplexParams, trials) should be (true)
  }

}
