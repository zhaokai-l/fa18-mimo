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
 * Define entry point for FFT FSM generator
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
    K = 2,
    M = 4,
    F = 33,
    O = 1,
  )
  val (chiselArgs, params) = argParse(args.toList, defaultParams)
  // Run the Chisel driver to generate a FSM
  Driver.execute(chiselArgs.toArray, () => new FFTFSM(params))
}

/**
  * Define entry point for Golay FSM generator
  */
object GolayFSMApp extends App {
  val usage = s"""FSM arguments:
                 |--w <Int>\t\tWidth of all IO
                 |--c <Int>\t\tNumber of correlation samples
                 |--k <Int>\t\tNumber of UEs
                 |--o <Int>\t\tOversampling ratio
                 |""".stripMargin
  /**
    * Parse arguments
    *
    * Some arguments are used by the GolayFSM generator and are used to construct a FixedGolayFSMParams object.
    * The rest get returned as a List[String] to pass to the Chisel driver
    *
    */
  def argParse(args: List[String], params: FixedGolayFSMParams): (List[String], FixedGolayFSMParams) = {
    args match {
      case "--help" :: tail =>
        println(usage)
        val (newArgs, newParams) = argParse(tail, params)
        ("--help" +: newArgs, newParams)
      case "--w" :: Int(w) :: tail => argParse(tail, params.copy(IOWidth = w))
      case "--c" :: Int(c) :: tail => argParse(tail, params.copy(C = c))
      case "--k" :: Int(k) :: tail => argParse(tail, params.copy(K = k))
      case "--o" :: Int(o) :: tail => argParse(tail, params.copy(O = o))
      case chiselOpt :: tail => {
        val (newArgs, newParams) = argParse(tail, params)
        (chiselOpt +: newArgs, newParams)
      }
      case Nil => (args, params)
    }
  }
  val defaultParams = FixedGolayFSMParams(
    IOWidth = 16,
    C = 1,
    K = 2,
    O = 1,
  )
  val (chiselArgs, params) = argParse(args.toList, defaultParams)
  // Run the Chisel driver to generate a FSM
  Driver.execute(chiselArgs.toArray, () => new GolayFSM(params))
}
