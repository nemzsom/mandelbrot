package hu.nemzsom.mandelbrot

import scala.annotation.tailrec
import scala.concurrent._
import scala.math._
import rx.lang.scala.{Subscription, Observer, Observable}

/**
 * It calculate an area's points.
 */
class Calculator(mainArea: Area, plotter: Plotter)(implicit val ec: ExecutionContext) {

  private object Config {
    val maxDividableSize = 17
    val iterationStep = 10
  }

  import Config._

  case class CalcStat(val all: Int, val settled: Int, val maxIter: Int)

  private class IterationCycle(val maxIter: Int) {

    lazy val next: IterationCycle = new IterationCycle(maxIter + iterationStep)
  }

  private val subscription: Subscription = new Subscription {}

  def calculate(): Observable[CalcStat] = Observable.create(
    (observer: Observer[CalcStat]) => {
      subscription
    }
  )

  private def calc(area: Area, cycle: IterationCycle): Unit = {
    val (CalcStat(pointsCount, settled, _), _) = update(area.borders, cycle)
    // uniform Unsettled borders
    if (settled == 0) {
      calc(area, cycle.next)
    }
    // uniform Outside borders
    else if (pointsCount == settled && isUniform(area.borders)) {
      val loc = area.topLeft.location
      area.subArea(1,1, area.width - 2, area.height - 2).foreach { point =>
        point.location = loc
        plotter.plot(point)
      }
    }
    else if (area.width > maxDividableSize || area.height > maxDividableSize)
  }

  private def isUniform(points: Traversable[Point]): Boolean =
    points.forall(_.location == points.head.location)

  private class Updater(points: Traversable[Point], cycle: IterationCycle) {

    private def update(): (CalcStat, Traversable[Point]) = {
      var pointsCount = 0
      var settled = 0
      var unsettledPoints = List.empty[Point]
      points.foreach { point =>
        pointsCount += 1
        iterate(point, cycle.maxIter)
        if (point.location == Unsettled){
          unsettledPoints = point :: unsettledPoints
        }
        else {
          settled += 1
          plotter.plot(point)
        }
      }
      (CalcStat(pointsCount, settled, cycle.maxIter), unsettledPoints)
    }

    /**
     * Performs the mandelbrot iteration on one point to the specified maxIteration.
     * @param point to update
     * @return true if the point escaped
     */
    protected def iterate(point: Point, maxIter: Int): Unit = if (point.iter < maxIter) {
      val c = point.complexValue
      if (point.iter == 0 && preCheck_isInside(c)){
        point.location = Inside
      }
      else {
        @tailrec def loop(iter: Int, z: Complex): Unit = {
          val escaped = z.escaped
          if (iter == maxIter || escaped) {
            point.iter = iter
            point.iterValue = z
            if (escaped) point.location = Outside(iter)
          }
          else loop(iter + 1, z * z + c)
        }
        val z = point.iterValue
        loop(point.iter + 1, z * z + c)
      }
    }

    /** Optimization: Cardioid / bulb checking
      * from http://en.wikipedia.org/wiki/Mandelbrot_set#Cardioid_.2F_bulb_checking
      */
    private def preCheck_isInside(c: Complex): Boolean = {
      val q = pow(c.re - 0.25, 2) + pow(c.im, 2)
      q * (q + (c.re - 0.25)) < pow(c.im, 2) / 4 ||
        pow(c.re + 1, 2) + pow(c.im, 2) < 0.0625
    }

    implicit class ComplexOps(c: Complex) {

      def escaped: Boolean = c.re * c.re + c.im * c.im > 2 * 2
    }
  }

  /*class Updater private(val area: Area, val points: Traversable[Point], val maxIter: Int)(implicit ec: ExecutionContext) {

    lazy val update: Future[Updater] = future {
      // debug BEGIN
//      print(s"UPDATE $this")
//      area.foreach( point => pixels(point.index) = 255 << 16)
//      painter.repaint()
//      debugQuene.take
      // debug END
      val unsettledPoints = updateAll()
      // debug BEGIN
//      area foreach color
//      println(" DONE")
      // debug END
      //painter.repaint() // TODO repaint only after a whole update cycle
      new Updater(area, unsettledPoints, maxIter + iterationStep)
    }

    /**
     * Updates one point to maxIter. The default implementation expects only [[hu.nemzsom.mandelbrot.Unsettled]] points
     * @param point to update
     */
    protected def updatePoint(point: Point): Unit = {
      iterate(point)
    }

    override def toString: String =
      s"Updater for $area, points size: ${points.size}, maxIter: $maxIter, id: ${hashCode}"

  }

  object Updater {

    def apply(area: Area)(implicit ec: ExecutionContext): Updater = apply(area, iterationStep)

    def apply(area: Area, maxIter: Int)(implicit ec: ExecutionContext): Updater = new Updater(area, area, maxIter) {

    // It handles any Point
    override def updatePoint(point: Point): Unit = {
      if (point.location == Unsettled) {
        if (point.iter == 0 && preCheck_isInside(point.complexValue)) point.location = Inside
        else super.updatePoint(point)
      }
    }
  }
}*/

}




