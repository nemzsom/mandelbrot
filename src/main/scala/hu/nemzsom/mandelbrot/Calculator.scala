package hu.nemzsom.mandelbrot

import scala.annotation.tailrec
import scala.concurrent._
import scala.math._
import rx.lang.scala.{Subscription, Observer, Observable}
import java.util.concurrent.atomic.AtomicInteger

/**
 * It calculates an area's points.
 */
class Calculator(mainArea: Area, plotter: Plotter)(implicit ec: ExecutionContext) {

  private object Config {
    val maxDividableSize = 17
    val iterationStep = 10
  }

  import Config._

  case class CalcStat(total: Int, settled: Int, maxIter: Int)

  /**
   * Aggregates the various update processes result statistics in one iteration cycle.
   * @param maxIter maximum iteration in this cycle
   * @param observer the observer to notify at the end of this cycle
   */
  private class IterationCycle(val maxIter: Int, observer: Observer[CalcStat]) {

    /** Remaining points count in this cycle */
    private val _remaining_total = new AtomicInteger(mainArea.size)
    /** Settled points in this cycle */
    private val _count_settled = new AtomicInteger
    /** Already settled points at the beginning of this cycle. The previous cycle updates this when finished */
    private var _initially_settled = 0

    /** The next cycle */
    lazy val next: IterationCycle = new IterationCycle(maxIter + iterationStep, observer)

    /** It called when an update process finishes this cycle
      * @param total total points updated by the process
      * @param settled settled points count
      */
    def updated(total: Int, settled: Int): Unit = {
      _count_settled.addAndGet(settled)
      check(_remaining_total.addAndGet(-total))
    }

    private def previousFinished(initiallySettled: Int): Unit =
      if (initiallySettled != 0) {
        _initially_settled = initiallySettled
        check(_remaining_total.addAndGet(-initiallySettled))
      }

    private def check(remaining: Int): Unit = {
      assert(remaining >= 0, s"remaining: $remaining") // TODO remove after debug
      if (remaining == 0) {
        val settled = _count_settled.get
        observer.onNext(CalcStat(mainArea.size - _initially_settled, settled, maxIter))
        next.previousFinished(_initially_settled + settled)
      }
    }
  }

  private val subscription: Subscription = new Subscription {}

  def calculate(): Observable[CalcStat] = Observable.create(
    (observer: Observer[CalcStat]) => {
      subscription
    }
  )

  private def calcWithBorder(area: Area, borderUpdater: Updater): Unit = {
    val border = borderUpdater.points
    borderUpdater.update().onSuccess { case (total, settled, nextUpdater) =>
      if (isUniform(border)){
        val loc = border.head.location
        val cycle = borderUpdater.cycle
        val areaSize = area.size
        if (loc == Unsettled) { // Unsettled border
          cycle.updated(areaSize, 0)
          calcWithBorder(area, nextUpdater)
        }
        else {                                    // Inside or outside (with same iter) border
          val middleArea = middle(area)
          middleArea.foreach{ point =>
            point.location = loc
            plotter.plot(point)
          }
          cycle.updated(areaSize, areaSize)
        }
      }
      else if (area.width > maxDividableSize || area.height > maxDividableSize) {

      }
    }
  }

  private def isUniform(points: Traversable[Point]): Boolean =
    points.forall(_.location == points.head.location)

  private def middle(area: Area) = area.subArea(1, 1, area.width - 2, area.height - 2)

  private class Updater private(val points: Traversable[Point], val cycle: IterationCycle)(implicit ec: ExecutionContext) {

    val maxIter = cycle.maxIter

    def update(): Future[(Int, Int, Updater)] = future {
      var total = 0
      var settled = 0
      var unsettledPoints = List.empty[Point]
      points.foreach { point =>
        if (updatePoint(point)) {
          total += 1
          if (point.location == Unsettled) {
            unsettledPoints = point :: unsettledPoints
          }
          else {
            settled += 1
            plotter.plot(point)
          }
        }
      }
      (total, settled, new Updater(unsettledPoints, cycle.next))
    }

    /**
     * Updates one point to maxIter. The default implementation expects only [[hu.nemzsom.mandelbrot.Unsettled]] points
     * @param point to update
     * @return true if the point counts as updated in the current calculation
     */
    protected def updatePoint(point: Point): Boolean = {
      iterate(point)
      true
    }

    /**
     * Performs the mandelbrot iteration on one point to the specified maxIteration. It expects only [[hu.nemzsom.mandelbrot.Unsettled]] points
     * @param point to update
     */
    protected def iterate(point: Point): Unit =
      if (point.iter < maxIter) {
        val c = point.complexValue
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


    implicit class ComplexOps(c: Complex) {

      def escaped: Boolean = c.re * c.re + c.im * c.im > 2 * 2
    }

  }

  object Updater {

    /**
     * Creates a new updater, that can start the update process. The iterationCycle must be the first in the calculation
     * @param points the points to update
     * @param cycle the first iteration cycle
     */
    def start(points: Traversable[Point], cycle: IterationCycle)(implicit ec: ExecutionContext) = newUpdater(points, cycle, true)

    /**
     * Creates a new updater, that can continue the update process. The IterationCycle must be a continuation of a previous cycle
     * @param points the points to update
     * @param cycle the actual cycle
     */
    def continue(points: Traversable[Point], cycle: IterationCycle)(implicit ec: ExecutionContext) = newUpdater(points, cycle, false)

    /**
     * Creates a new updater
     * @param points the points to update
     * @param cycle the actual cycle
     * @param starter true, if this is the first IterationCycle in the calculation
     */
    private def newUpdater(points: Traversable[Point], cycle: IterationCycle, starter: Boolean)(implicit ec: ExecutionContext): Updater = new Updater(points, cycle) {

      // It handles any Point
      override def updatePoint(point: Point): Boolean = {
        if (point.location == Unsettled) {
          if (point.iter == 0 && preCheck_isInside(point.complexValue)) {
            point.location = Inside
            true
          }
          else super.updatePoint(point)
        }
        else starter
      }

      /** Optimization: Cardioid / bulb checking
        * from http://en.wikipedia.org/wiki/Mandelbrot_set#Cardioid_.2F_bulb_checking
        */
      private def preCheck_isInside(c: Complex): Boolean = {
        val q = pow(c.re - 0.25, 2) + pow(c.im, 2)
        q * (q + (c.re - 0.25)) < pow(c.im, 2) / 4 ||
          pow(c.re + 1, 2) + pow(c.im, 2) < 0.0625
      }
    }
  }

}




