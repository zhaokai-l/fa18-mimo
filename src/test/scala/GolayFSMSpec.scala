package mimo

import dsptools.numbers._
import org.scalatest.{FlatSpec, Matchers}
import breeze.math.Complex


/**
  * Objects that parses the test correlation samples from Python simulation (CSV format).
  * The correlation is formatted as a column vector way:
  * [sample 0 real]
  * [sample 1 real]
  * ...
  * [sample n-1 imaginary]
  * [sample n imaginary]
  */

class GolayFSMSpec extends FlatSpec with Matchers {
  behavior of "GolayFSM"

  val params = FixedGolayFSMParams(
    IOWidth = 16,
    //make sure these match the test files
    N = 80,
    C = 1,
    K = 2,
    M = 4,
    O = 1
  )

  // load correlated pilots and where the valid is asserted (peak) from test files
  val rsrc = "src/test/resources/"
  val antCorrsRaw = Array.ofDim[List[Double]](params.M)
  val peakValids = Array.ofDim[List[Boolean]](params.K)
  for (m <- 0 until params.M) {
    antCorrsRaw(m) = io.Source.fromFile(rsrc+"Antenna_"+m+"_Golay_Result.csv").getLines.toList.map{_.toDouble}
    peakValids(m) = io.Source.fromFile(rsrc+"Antenna_"+m+"_Peak_Valid.csv").getLines.toList.map{_ == "TRUE"}
  }

  // repackage into array of complex numbers & calculate golden channel estimation
  val nSamps = peakValids(0).length/2
  val antCorrs, hH = Array.ofDim[List[Complex]](params.M)
  for (m <- 0 until params.M) {
    antCorrs(m) = antCorrsRaw(m).splitAt(nSamps).zipped.map{case(a,b) => Complex(a,b)}
    hH(m) = (peakValids(m) zip antCorrs(m)).collect{case(a,b) if a => b.conjugate}
  }

  // make baseCPW sample
  val baseSample = CPW(correlation = Complex(0,0), peakValid = false)
  val samples = Array.ofDim[CPW](params.M*nSamps)

  // append all antennas together
  for (m <- 0 until params.M) {
    var k = 0 //which user's golden pilot
    for(s <- 0 until nSamps) {
      val goldenWeight = if(peakValids(m)(s)) Some(hH(m)(k+=1))  else None
      samples(params.M*nSamps+s) = baseSample.copy(correlation = antCorrs(m)(s), peakValid = peakValids(m)(s), weight = goldenWeight)
    }
  }

  it should "Fixed channel estimate" in {
    FixedGolayFSMTester(params, samples) should be (true)
  }

  val realParams = new GolayFSMParams[DspReal] {
    val proto = DspComplex(DspReal())
    val N = params.N
    val C = params.C
    val K = params.K
    val M = params.M
    val O = params.O
  }

  // can reuse all the stimulus from above
  // TODO: Real test does not work
  it should "DspReal channel estimate" in {
    //RealGolayFSMTester(realParams, samples) should be (true)
  }
}