package mimo

import dsptools.numbers._
import org.scalatest.{FlatSpec, Matchers}

class PESpec extends FlatSpec with Matchers {
  behavior of "PE"

  val realParams = new PEParams[DspReal] {
    val inA = DspReal()
    val inB = DspReal()
    val outA = DspReal()
    val outB = DspReal()
    val outC = DspReal()
    val nIters = 8
  }
  it should "multiply" in {
    val baseTrial = ABC(ain=Seq(1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0), bin=Seq(1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0), cout=Some(8.0))
    //val b = Seq(-1, -0.5, 0, 0.25, 0.5, 1, 2, 3)
    //val trials = b.map { bs => baseTrial.copy(bin = bs, cout = Some(b.fold(0)(_ + _))) }
    RealPETester(realParams, Seq(baseTrial)) should be (true)
  }

}
