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
    C = 1,
    K = 2,
    O = 1
  )

  // load correlated pilots and where the valid is asserted (peak) from test files
  val M = 4 // # of antennas
  val rsrc = "src/test/resources/"
  val antCorrsRaw = Array.ofDim[List[Double]](M)
  val peakValids = Array.ofDim[List[Boolean]](M)
  for (m <- 0 until M) {
    antCorrsRaw(m) = io.Source.fromFile(rsrc+"Antenna_"+m+"_Golay_Result.csv").getLines.toList.map{_.toDouble}
    peakValids(m) = io.Source.fromFile(rsrc+"Antenna_"+m+"_Peak_Valid.csv").getLines.toList.map{_ == "TRUE"}
  }

  // repackage into array of complex numbers & calculate golden Hermitian conjugates
  val nSamps = peakValids(0).length
  val antCorrs, hH = Array.ofDim[List[Complex]](M)
  for (m <- 0 until M) {
    antCorrs(m) = antCorrsRaw(m).splitAt(nSamps).zipped.map{case(r,i) => Complex(r,i)}
    hH(m) = antCorrs(m).map{_.conjugate}
  }

  // make baseCPW sample
  val baseSample = CPW(correlation = Complex(0,0), peakValid = false)
  val samples = Array.ofDim[CPW](M*nSamps)

  // append all antennas together
  for (m <- 0 until M) {
    for(s <- 0 until nSamps) {
      val goldenWeight = if(peakValids(m)(s)) Some(hH(m)(s)) else None
      samples(m*nSamps+s) = baseSample.copy(correlation = antCorrs(m)(s), peakValid = peakValids(m)(s), weight = goldenWeight)
    }
  }

  it should "Fixed channel estimate" in {
    FixedGolayFSMTester(params, samples) should be (true)
  }

  val realParams = new GolayFSMParams[DspReal] {
    val proto = DspComplex(DspReal())
    val C = params.C
    val K = params.K
    val O = params.O
  }

  // can reuse all the stimulus from above
  it should "DspReal channel estimate" in {
    RealGolayFSMTester(realParams, samples) should be (true)
  }
}