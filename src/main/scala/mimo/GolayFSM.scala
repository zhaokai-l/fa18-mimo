package mimo

import chisel3._
import chisel3.experimental.FixedPoint
import chisel3.util.Decoupled
import dsptools.numbers._
import freechips.rocketchip.diplomacy.LazyModule
import freechips.rocketchip.subsystem.BaseSubsystem

/**
 * Base class for GolayFSM parameters
 *
 * These are type generic
 */
trait GolayFSMParams[T <: Data] {
  val protoXY: T
  val protoZ: T
  val nStages: Int
  val correctGain: Boolean
  val stagesPerCycle: Int
}

/**
 * GolayFSM parameters object for fixed-point GolayFSMs
 */
case class FixedGolayFSMParams(
  // width of X and Y
  xyWidth: Int,
  // width of Z
  zWidth: Int,
  // scale output by correction factor?
  correctGain: Boolean = true,
  // number of GolayFSM stages to perform per clock cycle
  stagesPerCycle: Int = 1,
) extends GolayFSMParams[FixedPoint] {
  // prototype for x and y
  // binary point is (xyWidth-2) to represent 1.0 exactly
  val protoXY = FixedPoint(xyWidth.W, (xyWidth-2).BP)
  // prototype for z
  // binary point is (xyWidth-3) to represent Pi/2 exactly
  val protoZ = FixedPoint(zWidth.W, (zWidth-2).BP)
  val minNumber = math.pow(2.0, -(zWidth-2))
  // number of GolayFSM stages
  private var n = 0
  while (breeze.numerics.tan(math.pow(2.0, -n)) >= minNumber) {
    n += 1
  }
  val nStages = n
}

/**
 * Bundle type that describes the input, state, and output of GolayFSM
 */
class GolayFSMBundle[T <: Data](params: GolayFSMParams[T]) extends Bundle {
  val x: T = params.protoXY.cloneType
  val y: T = params.protoXY.cloneType
  val z: T = params.protoZ.cloneType

  override def cloneType: this.type = GolayFSMBundle(params).asInstanceOf[this.type]
}
object GolayFSMBundle {
  def apply[T <: Data](params: GolayFSMParams[T]): GolayFSMBundle[T] = new GolayFSMBundle(params)
}

/**
 * Bundle type as IO for iterative GolayFSM modules
 */
class IterativeGolayFSMIO[T <: Data](params: GolayFSMParams[T]) extends Bundle {
  val in = Flipped(Decoupled(GolayFSMBundle(params)))
  val out = Decoupled(GolayFSMBundle(params))

  val vectoring = Input(Bool())

  override def cloneType: this.type = IterativeGolayFSMIO(params).asInstanceOf[this.type]
}
object IterativeGolayFSMIO {
  def apply[T <: Data](params: GolayFSMParams[T]): IterativeGolayFSMIO[T] =
    new IterativeGolayFSMIO(params)
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
trait HasPeripheryGolayFSM extends BaseSubsystem {
  // instantiate GolayFSM chain
  val GolayFSMChain = LazyModule(new GolayFSMThing(FixedGolayFSMParams(8, 10)))
  // connect memory interfaces to pbus
  pbus.toVariableWidthSlave(Some("GolayFSMWrite")) { GolayFSMChain.writeQueue.mem.get }
  pbus.toVariableWidthSlave(Some("GolayFSMRead")) { GolayFSMChain.readQueue.mem.get }
}
