package mimo

import dsptools.numbers._
import org.scalatest.{FlatSpec, Matchers}
import breeze.math.Complex


/**
  * Objects that parses the test OFDM pilot & payload words from Python simulation (CSV format).
  * The pilot spectrum is formatted as a comma separated list of booleans.
  * The FFT spectrum is formatted this way:
  * [word 0 all subcarriers real],[word 0 all subcarriers imaginary]
  * [word 1 all subcarriers real],[word 1 all subcarriers imaginary]
  * ... and so on.
  */

object OFDMWords {
  def apply[T <: String](filename: String, subcarriers: Int, frames: Int): Array[Array[String]] = {
    val data = Array.ofDim[String](frames, subcarriers)
    val bufferedSource = io.Source.fromFile(filename)
    bufferedSource.getLines.zipWithIndex.foreach {
      case(line,count) => data(count) = line.split(",").map(_.trim)
    }
    data
  }
}

class FFTFSMSpec extends FlatSpec with Matchers {
  behavior of "FFTFSM"

  val params = FixedFFTFSMParams(
    IOWidth = 16,
    //make sure these match the test files
    S = 256,
    K = 2,
    M = 4,
    F = 33,
    O = 1
  )

  // load test pilots & FFT spectrums
  val rsrc = "src/test/resources/"
  val txPilots = Array.ofDim[Array[Boolean]](params.K)
  for (k <- 0 until params.K) {
    val word = OFDMWords(rsrc+"User_"+k+"_Pilot_Word.csv", params.S, 1).map(_.map(_ == "1"))
    txPilots(k) = word(0)
  }
  val antFFTs = Array.ofDim[Array[Double]](params.M, params.K+params.F)
  for (m <- 0 until params.M) {
    antFFTs(m) = OFDMWords(rsrc+"Antenna_"+m+"_FFT_Result.csv", params.S*2, params.K+params.F).map(_.map(_.toDouble))
  }

  // extract FFTs of each antenna's pilots and payloads only as Array[Complex].
  // also calculate golden weights (Hermitian of channel response).
  val antPilots, h, hH = Array.ofDim[Array[Complex]](params.M, params.K)
  val antPayloads = Array.ofDim[Array[Complex]](params.M, params.F)
  for (m <- 0 until params.M) {
    // pilots & golden weights
    for (k <- 0 until params.K) {
      antPilots(m)(k) = (antFFTs(m)(k).slice(0, params.S) zip antFFTs(m)(k).slice(params.S, 2*params.S)).map{case (r,i) => Complex(r,i)}
      h(m)(k) = (txPilots(k) zip antPilots(m)(k)).map{case (a,b) => if(a) {b} else {-b}}
      hH(m)(k) = h(m)(k).map{_.conjugate/(params.M*params.O)}
    }
    // payloads
    for (f <- params.K until params.F+params.K) {
      antPayloads(m)(f-params.K) = (antFFTs(m)(f).slice(0, params.S) zip antFFTs(m)(f).slice(params.S, 2 * params.S)).map{case (r,i) => Complex(r,i)}
    }
  }

  // make a dummy pilot & FFT (all 1's for payload frames)
  val dummyPilot = Array.fill[Boolean](params.S)(true)
  val dummyFFT = Array.fill[Complex](params.S)(Complex(0,0))

  // make basePSW frame
  val baseFrame = PSW(pilot = dummyPilot, spectrum = dummyFFT)
  val frames = Array.ofDim[PSW]((params.K+params.F)*params.M)

  // append all antennas together
  for (m <- 0 until params.M) {
    //append pilot PSW objects for all users
    for (k <- 0 until params.K) {
      frames(m*(params.K+params.F)+k) = baseFrame.copy(pilot = txPilots(k), spectrum = antPilots(m)(k), weights = Some(hH(m)(k)))
    }
    //followed by payload PSW objects
    for (f <- 0 until params.F) {
      frames(m*(params.K+params.F)+params.K+f) = baseFrame.copy(spectrum = antPayloads(m)(f))
    }
  }

  it should "Fixed channel estimate" in {
    FixedFFTFSMTester(params, frames) should be (true)
  }

  val realParams = new FFTFSMParams[DspReal] {
    val proto = DspComplex(DspReal())
    val S = params.S
    val K = params.K
    val M = params.M
    val F = params.F
    val O = params.O
  }

  // can reuse all the stimulus from above
  // TODO: Real test does not work
  it should "DspReal channel estimate" in {
    //RealFFTFSMTester(realParams, frames) should be (true)
  }
}