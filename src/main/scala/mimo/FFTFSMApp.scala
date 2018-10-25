package mimo

import chisel3._

/**
  * Make an unapply function for the argument parser.
  * It allows us to match on parameters that are integers
  */
object Int {
  def unapply(v: String): Option[Int] = {
    try {
      Some(v.toInt)
    } catch {
      case _: NumberFormatException => None
    }
  }
}


/**
 * Define entry point for FSM generator
 */
object FFTFSMApp extends App {
  val usage = s"""FSM arguments:
  |--w <Int>\t\tWidth of all IO
  |--s <Int>\t\tNumber of subcarriers
  |--k <Int>\t\tNumber of UEs
  |--m <Int>\t\tNumber of antennas
  |--f <Int>\t\tNumber of payload frames (symbols)
  |--o <Int>\t\tOversampling ratio
  |""".stripMargin
  /**
   * Parse arguments
   *
   * Some arguments are used by the FFTFSM generator and are used to construct a FixedFFTFSMParams object.
   * The rest get returned as a List[String] to pass to the Chisel driver
   *
   */
  def argParse(args: List[String], params: FixedFFTFSMParams): (List[String], FixedFFTFSMParams) = {
    args match {
      case "--help" :: tail =>
        println(usage)
        val (newArgs, newParams) = argParse(tail, params)
        ("--help" +: newArgs, newParams)
      case "--w" :: Int(w) :: tail => argParse(tail, params.copy(IOWidth = w))
      case "--s" :: Int(s) :: tail => argParse(tail, params.copy(S = s))
      case "--k" :: Int(k) :: tail => argParse(tail, params.copy(K = k))
      case "--m" :: Int(m) :: tail => argParse(tail, params.copy(M = m))
      case "--f" :: Int(f) :: tail => argParse(tail, params.copy(F = f))
      case "--o" :: Int(o) :: tail => argParse(tail, params.copy(O = o))
      case chiselOpt :: tail => {
        val (newArgs, newParams) = argParse(tail, params)
        (chiselOpt +: newArgs, newParams)
      }
      case Nil => (args, params)
    }
  }
  val defaultParams = FixedFFTFSMParams(
    IOWidth = 16,
    S = 256,
    K = 1,
    M = 16,
    F = 256,
    O = 1,
  )
  val (chiselArgs, params) = argParse(args.toList, defaultParams)
  // Run the Chisel driver to generate a FSM
  Driver.execute(chiselArgs.toArray, () => new FFTFSM(params))
}
