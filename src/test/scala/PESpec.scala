package mimo

import dsptools.numbers._
import org.scalatest.{FlatSpec, Matchers}
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
  it should "multiply" in {
    val numTrials = 10

    val trials = Seq.fill(numTrials) {
      val a = Seq.fill(realParams.nIters)(Random.nextDouble())
      val b = Seq.fill(realParams.nIters)(Random.nextDouble())
      val c = (a zip b).map { case (a: Double, b: Double) => a * b }.sum
      ABC(ain=a, bin=b, cout=Some(c))
    }

    RealPETester(realParams, trials) should be (true)
  }

}
