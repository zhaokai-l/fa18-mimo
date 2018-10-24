package cordic

import chisel3._
import chisel3.experimental.FixedPoint
import chisel3.util.Decoupled
import chisel3.util._
import dsptools.numbers._
import freechips.rocketchip.diplomacy.LazyModule
import freechips.rocketchip.subsystem.BaseSubsystem

/**
  * Base class for CORDIC parameters
  *
  * These are type generic
  */
trait CordicParams[T <: Data] {
  val protoXY: T
  val protoZ: T
  val nStages: Int
  val correctGain: Boolean
  val stagesPerCycle: Int
}


/**
  * CORDIC parameters object for fixed-point CORDICs
  */
case class FixedCordicParams(
  // width of X and Y
  xyWidth: Int,
  // width of Z
  zWidth: Int,
  // scale output by correction factor?
  correctGain: Boolean = true,
  // number of CORDIC stages to perform per clock cycle
  stagesPerCycle: Int = 1,
) extends CordicParams[FixedPoint] {
  // prototype for x and y
  // binary point is (xyWidth-2) to represent 1.0 exactly
  val protoXY = FixedPoint(xyWidth.W, (xyWidth-2).BP)
  // prototype for z
  // binary point is (xyWidth-3) to represent Pi/2 exactly
  val protoZ = FixedPoint(zWidth.W, (zWidth-2).BP)
  val minNumber = math.pow(2.0, -(zWidth-2))
  // number of cordic stages
  private var n = 0
  while (breeze.numerics.tan(math.pow(2.0, -n)) >= minNumber) {
    n += 1
  }
  val nStages = n
}





/**
  * Bundle type that describes the input, state, and output of CORDIC
  */
class CordicBundle[T <: Data](params: CordicParams[T]) extends Bundle {
  val x: T = params.protoXY.cloneType
  val y: T = params.protoXY.cloneType
  val z: T = params.protoZ.cloneType

  override def cloneType: this.type = CordicBundle(params).asInstanceOf[this.type]
}
object CordicBundle {
  def apply[T <: Data](params: CordicParams[T]): CordicBundle[T] = new CordicBundle(params)
}

class CordicBundleWithVectoring[T <: Data](params: CordicParams[T]) extends CordicBundle[T](params) {
  val vectoring: Bool = Bool()
  override def cloneType: this.type = CordicBundleWithVectoring(params).asInstanceOf[this.type]
}
object CordicBundleWithVectoring {
  def apply[T <: Data](params: CordicParams[T]): CordicBundleWithVectoring[T] = new CordicBundleWithVectoring(params)
}

/**
  * Bundle type as IO for iterative CORDIC modules
  */
class IterativeCordicIO[T <: Data](params: CordicParams[T]) extends Bundle {
  val in = Flipped(Decoupled(CordicBundleWithVectoring(params)))
  val out = Decoupled(CordicBundle(params))

  //val vectoring = Input(Bool())

  override def cloneType: this.type = IterativeCordicIO(params).asInstanceOf[this.type]
}
object IterativeCordicIO {
  def apply[T <: Data](params: CordicParams[T]): IterativeCordicIO[T] =
    new IterativeCordicIO(params)
}

object AddSub {
  def apply[T <: Data : Ring](sel: Bool, a: T, b: T): T = {
    Mux(sel, a + b, a - b)
  }
}


/**
  * Mixin for top-level rocket to add a PWM
  *
  */
trait HasPeripheryCordic extends BaseSubsystem {
  // instantiate cordic chain
  val cordicChain = LazyModule(new CordicThing(FixedCordicParams(8, 10)))
  // connect memory interfaces to pbus
  pbus.toVariableWidthSlave(Some("cordicWrite")) {
    cordicChain.writeQueue.mem.get
  }
  pbus.toVariableWidthSlave(Some("cordicRead")) {
    cordicChain.readQueue.mem.get
  }
}



//class IterativeCordic(val params: CordicParams[FixedPoint]) extends Module {
class IterativeCordic[T <: Data : Real](params: CordicParams[T]) extends Module {
  val io = IO(IterativeCordicIO(params))
  // Decoupled signals
  io.out.valid := false.B
  io.in.ready := false.B

  // Output registers
  val x = Reg(params.protoXY)
  val y = Reg(params.protoXY)
  val z = Reg(params.protoZ)

  // Vectoring parameter
  val vec = Reg(Bool())

  // Iteration count
  val i = RegInit(UInt(32.W), 0.U)

  // ROM values
  val tans = VecInit(Constants.arctan(params.nStages).map(ConvertableTo[T].fromDouble(_)))
  val lins = VecInit(Constants.linear(params.nStages).map(ConvertableTo[T].fromDouble(_)))
  val gain = ConvertableTo[T].fromDouble(1/Constants.gain(params.nStages))


  when (i === 0.U && io.in.valid) {
    // Initial state where input is read in.
    z := io.in.bits.z
    y := io.in.bits.y
    x := io.in.bits.x
    io.in.ready := true.B
    io.out.valid := false.B
    vec := io.in.bits.vectoring
    i := i + 1.U

  } .elsewhen (i > 0 && i < params.nStages.U) {
    // Iteration stage of CORDIC algorithm.
    io.in.ready := false.B

    // Choose between vectoring and rotation modes
    var d = Mux(vec,
      Mux(y > 0, ConvertableTo[T].fromDouble(-1.0), ConvertableTo[T].fromDouble(1)),
      Mux(z < 0, ConvertableTo[T].fromDouble(-1.0), ConvertableTo[T].fromDouble(1)))

    // Main CORDIC calculation
    var zi = z - d * tans(i - 1.U)
    var yi = y + x * d * lins(i - 1.U)
    var xi = x - y * d * lins(i - 1.U)

    // Unrolling if wanted
    for (iter <- 1 until params.stagesPerCycle) {
      d = Mux(vec,
        Mux(yi > 0, ConvertableTo[T].fromDouble(-1.0), ConvertableTo[T].fromDouble(1.0)),
        Mux(zi < 0, ConvertableTo[T].fromDouble(-1.0), ConvertableTo[T].fromDouble(1.0)))
      zi = zi - d * tans(i - 1.U + iter.U)
      val yi_temp = yi + xi * d * lins(i - 1.U + iter.U)
      xi = xi - yi * d * lins(i - 1.U + iter.U)
      yi = yi_temp
    }
    z := zi
    y := yi
    x := xi

    // Update iteration count
    i := i + params.stagesPerCycle.U

  } .elsewhen (i >= params.nStages.U) {
    // Algorithm is done, so get ready for the next input.
    i := 0.U
    io.out.valid := true.B
    io.in.ready := false.B
  } .otherwise {
    i := 0.U
  }

  // Set output, with or without gain correction
  io.out.bits.z := z
  if (params.correctGain) {
    io.out.bits.y := y * gain
    io.out.bits.x := x * gain
  } else {
    io.out.bits.y := y
    io.out.bits.x := x
  }


}
