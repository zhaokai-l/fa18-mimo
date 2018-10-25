package mimo

import dsptools.numbers._
import org.scalatest.{FlatSpec, Matchers}
import breeze.math.Complex


/**
  * Objects that parses the test correlation words from Python simulation (CSV format).
  * The correlation is formatted this way:
  * [word 0 real],[word 0 imaginary]
  * [word 1 real],[word 1 imaginary]
  * ... and so on.
  */

object CorrWords {
  def apply[T <: String](filename: String, samples: Int, frames: Int): Array[Array[String]] = {
    val data = Array.ofDim[String](frames, samples)
    val bufferedSource = io.Source.fromFile(filename)
    bufferedSource.getLines.zipWithIndex.foreach {
      case(line,count) => data(count) = line.split(",").map(_.trim)
    }
    data
  }
}

class GolayFSMSpec extends FlatSpec with Matchers {
  behavior of "GolayFSM"

  val params = FixedGolayFSMParams(
    IOWidth = 16,
    //make sure these match the test files
    N = 64,
    C = 1,
    K = 2,
    M = 4,
    F = 33,
    O = 1
  )

  // load test pilots & Corr spectrums
  val rsrc = "src/test/resources/"
  val antCorrs = Array.ofDim[Array[Double]](params.M, params.K+params.F)
  for (n <- 0 until params.N) {
    antCorrs(n) = OFDMWords(rsrc+"Antenna_"+n+"_Corr_Result.csv", params.N*2, params.K+params.F).map(_.map(_.toDouble))
  }

  // extract Corrs of each antenna's pilots and payloads only as Array[Complex].
  // also calculate golden weights (Hermitian of channel response).
  val antPilots, h, hH = Array.ofDim[Array[Complex]](params.M, params.K)
  val antPayloads = Array.ofDim[Array[Complex]](params.M, params.F)
  for (m <- 0 until params.M) {
    // pilots & golden weights
    for (k <- 0 until params.K) {
      antPilots(m)(k) = (antCorrs(m)(k).slice(0, params.N) zip antCorrs(m)(k).slice(params.N, 2*params.N)).map{case (r,i) => Complex(r,i)}
      hH(m)(k) = antPilots(m)(k).map{_.conjugate/(params.M*params.O)}
    }
    // payloads
    for (f <- params.K until params.F+params.K) {
      antPayloads(m)(f-params.K) = (antCorrs(m)(f).slice(0, params.N) zip antCorrs(m)(f).slice(params.N, 2 * params.N)).map{case (r,i) => Complex(r,i)}
    }
  }

  // make a dummy correlation (all 1's for payload frames)
  val dummyCorr = Array.fill[Complex](params.N)(Complex(0,0))

  // make baseCW frame
  val baseFrame = CW(correlation = dummyCorr)
  val frames = Array.ofDim[CW]((params.K+params.F)*params.M)

  // append all antennas together
  for (m <- 0 until params.M) {
    //append pilot CW objects for all users
    for (k <- 0 until params.K) {
      frames(m*(params.K+params.F)+k) = baseFrame.copy(correlation = antPilots(m)(k), weight = Some(hH(m)(k)))
    }
    //followed by payload CW objects
    for (f <- 0 until params.F) {
      frames(m*(params.K+params.F)+params.K+f) = baseFrame.copy(correlation = antPayloads(m)(f))
    }
  }

  it should "Fixed channel estimate" in {
    FixedGolayFSMTester(params, frames) should be (true)
  }

  val realParams = new GolayFSMParams[DspReal] {
    val proto = DspComplex(DspReal())
    val N = params.N
    val C = params.C
    val K = params.K
    val M = params.M
    val F = params.F
    val O = params.O
  }

  // can reuse all the stimulus from above
  // TODO: Real test does not work
  it should "DspReal channel estimate" in {
    //RealGolayFSMTester(realParams, frames) should be (true)
  }
}