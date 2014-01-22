package hu.n.zs.mandelbrot

import scala.annotation.tailrec
import java.util.concurrent.{Executors, ThreadPoolExecutor}
import scala.concurrent._
import scala.math._

object BorderPos extends Enumeration {
  type BorderPos = Value
  val TopLeft, Top, TopRight, Right, BottomRight, Bottom, BottomLeft, Left = Value
}

/**
 * It can calculate an area's points.
 */
trait Calculator extends Renderer {

  val mainArea: Area

  val numOfProcs = Runtime.getRuntime.availableProcessors
  val executor: ThreadPoolExecutor = Executors.newFixedThreadPool(numOfProcs *2).asInstanceOf[ThreadPoolExecutor]
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutor(executor)

 def calc(borders: Seq[Updater], middle: Updater) : Unit = {
   var time = System.nanoTime
   val updates = borders.map(_.update)
   Future.sequence(updates).onSuccess { case seq: Seq[Updater] =>
     println(s"All updated in ${(System.nanoTime - time) / 1000000} ms")
     time = System.nanoTime()
     for (updater <- seq) {
       updater.area.foreach(color)
     }
     painter.repaint() // TODO partial repaint?
     println(s"Rendered in ${(System.nanoTime - time) / 1000000} ms ")
   }
 }
}

class Updater private (val area: Area, val points: Traversable[Point], val iterationStep: Int, val maxIter: Int)(implicit val ec: ExecutionContext) {

  lazy val update: Future[Updater] = future {
    //println(s"Points to update at $maxIter: ${points.size}")
    val unsettledPoints = updateAll()
    new Updater(area, unsettledPoints, iterationStep, maxIter + iterationStep)
  }

  private def updateAll(): Traversable[Point] = {
    var unsettledPoints = List.empty[Point]
    points.foreach { point =>
      updatePoint(point)
      if (point.location == Unsettled)
        unsettledPoints = point :: unsettledPoints
    }
    unsettledPoints
  }

  /**
   * Updates one point to maxIter. The default implementation expects only [[hu.n.zs.mandelbrot.Unsettled]] points
   * @param point to update
   */
  protected def updatePoint(point: Point): Unit = {
    iterate(point)
  }

  /**
   * Performs the mandelbrot iteration on one point
   * @param point to update
   */
  protected def iterate(point: Point): Unit = {
    val c = point.complexValue
    @tailrec def loop(iter: Int, z: Complex): Unit = {
      val escaped = z.escaped
      if (iter == maxIter || escaped) {
        point.iter = iter
        point.iterValue = z
        if (escaped) point.location = Outside(iter)
      }
      else loop(iter+1, z*z + c)
    }
    val z = point.iterValue
    loop(point.iter+1, z*z + c)
  }

  /** Optimization: Cardioid / bulb checking
    *  from http://en.wikipedia.org/wiki/Mandelbrot_set#Cardioid_.2F_bulb_checking
    */
  def preCheck_isInside(c: Complex): Boolean = {
    val q = pow(c.re - 0.25, 2) + pow(c.im, 2)
    q*(q + (c.re - 0.25)) < pow(c.im, 2) / 4 ||
      pow(c.re + 1, 2) + pow(c.im, 2) < 0.0625
  }

  implicit class ComplexOps(c: Complex) {

    def escaped: Boolean = c.re*c.re + c.im*c.im > 2*2
  }
}

object Updater {

  def apply(area: Area, iterationStep: Int)(implicit ec: ExecutionContext) = new Updater(area, area, iterationStep, iterationStep) {

    // It handles any Point
    override def updatePoint(point: Point): Unit = {
      if (point.location == Unsettled){
        if (point.iter == 0 && preCheck_isInside(point.complexValue)) point.location = Inside
        else super.updatePoint(point)
      }
    }
  }
}


