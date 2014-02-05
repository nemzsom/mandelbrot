package hu.nemzsom.mandelbrot

import scala.annotation.tailrec
import scala.concurrent._
import scala.math._
import rx.lang.scala.{Subscription, Observer, Observable}
import java.util.concurrent.atomic.AtomicInteger

case class CalcStat(total: Int, settled: Int, maxIter: Int)

/**
 * It calculates an area's points.
 */
class Calculator(mainArea: Area, plotter: Plotter)(implicit ec: ExecutionContext) {

  private object Config {
    val maxDividableSize = 100
    val iterationStep = 1
  }

  import Config._

  /**
   * Aggregates the various update processes result statistics in one iteration cycle.
   * @param maxIter maximum iteration in this cycle
   * @param observer the observer to notify at the end of this cycle
   */
  class IterationCycle(val maxIter: Int, observer: Observer[CalcStat]) {

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
      // DEBUG
      println(s"$this update with total: $total, settled: $settled.")
      // DEBUG END
      _count_settled.addAndGet(settled)
      check(_remaining_total.addAndGet(-total))
    }

    private def previousFinished(initiallySettled: Int): Unit = {
      // DEBUG
      println(s"$this prev finished with initiallySettled: $initiallySettled.")
      // DEBUG END
      if (initiallySettled != 0) {
        _initially_settled = initiallySettled
        check(_remaining_total.addAndGet(-initiallySettled))
      }
    }

    private def check(remaining: Int): Unit = {
      // DEBUG
      println(s"Ccheck with remaining: $remaining.")
      // DEBUG END
      assert(remaining >= 0, s"remaining: $remaining") // TODO remove after debug
      if (remaining == 0) {
        val settled = _count_settled.get
        observer.onNext(CalcStat(mainArea.size - _initially_settled, settled, maxIter))
        next.previousFinished(_initially_settled + settled)
      }
    }

    override def toString = s"cycle($maxIter)[remaining: ${_remaining_total}, settled: ${_count_settled}, initially settled: ${_initially_settled}]"
  }

  private val subscription: Subscription = new Subscription {}

  def calculate(): Observable[CalcStat] = Observable.create(
    (observer: Observer[CalcStat]) => {
      // TODO call onCompleted on the observer WHEN ALL UPDATE FINISHED (either after unsubscribe or all points settled)
      // TODO divide initially to the number of processors
      // TODO if both size bigger than 4 on the complex plane, divide further (to defense against a uniform border entirely outside of the mandelbrot set)
      calcWithBorder(mainArea, Updater.start(mainArea.border, new IterationCycle(iterationStep, observer)))
      subscription
    }
  )

  private def calcWithBorder(area: Area, borderUpdater: Updater): Unit = if (!subscription.isUnsubscribed){
    val border = borderUpdater.points
    val cycle = borderUpdater.cycle
    borderUpdater.update().onSuccess { case next =>
      if (isUniform(border)) {
        handleUniformBorder(area, border, cycle, next)
      }
      else if (isDividable(area)) {
        divideAndCalc(area, cycle)
      }
      else {
        Updater.start(middle(area), cycle).update().onSuccess { case _ =>
          recurseCalc(Updater.continue(area, cycle.next))
        }
      }
    }
  }

  private def handleUniformBorder(area: Area, border: Traversable[Point], cycle: IterationCycle, next: Updater): Unit = {
    val loc = border.head.location
    val innerAreaSize = (area.width - 2) * (area.height - 2)
    if (loc == Unsettled) {         // Unsettled border
      cycle.updated(innerAreaSize, 0)
      calcWithBorder(area, next.update(), cycle.next)
    }
    else {                          // Inside or outside (with same iter) border
      val middleArea = middle(area)
      middleArea.foreach{ point =>
        point.location = loc
        plotter.plot(point)
      }
      cycle.updated(innerAreaSize, innerAreaSize)
    }
  }

  private def divideAndCalc(area: Area, cycle: IterationCycle): Unit = {
    def calc(a1: Area, a2: Area, dividingLine: Area): Unit = {
      Updater.start(dividingLine, cycle).update().onSuccess { case _ =>
        calcWithBorder(a1, Updater.continue(a1.border, cycle.next))
        calcWithBorder(a2, Updater.continue(a2.border, cycle.next))
      }
    }
    if (area.width > area.height) {
      val (left, right) = area.splitHorizontal
      val dividingLine = area.subArea(left.width - 1, 1, 2, area.height - 2)
      calc(left, right, dividingLine)
    }
    else {
      val (top, bottom) = area.splitVertical
      val dividingLine = area.subArea(1, top.height - 1, area.width - 2, 2)
      calc(top, bottom, dividingLine)
    }
  }

  private def recurseCalc(updater: Updater): Unit = if (!subscription.isUnsubscribed) {
    updater.update().onSuccess { case nextUpdater =>
      if (nextUpdater.hasPoints) recurseCalc(nextUpdater)
    }
  }

  private def isDividable(area : Area): Boolean =
    area.width >= maxDividableSize || area.height >= maxDividableSize

  private def isUniform(points: Traversable[Point]): Boolean =
    points.forall(_.location == points.head.location)

  private def middle(area: Area) = area.subArea(1, 1, area.width - 2, area.height - 2)

  class Updater private(val points: Traversable[Point], val cycle: IterationCycle)(implicit ec: ExecutionContext) {

    val maxIter = cycle.maxIter

    // TODO cycle.next and Updater.update does similar things. The logic should be in one place.
    def update(): Future[Updater] = future {
      // debug BEGIN
      print(s"UPDATE at $cycle")
      points.foreach( point => debugPixels(point.index) = 255 << 16)
      debugPanel.repaint()
      debugQuene.take
      // debug END
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
      // debug BEGIN
      points foreach plotter.plot
      println(s" DONE. total: $total, settled: $settled")
      debugPanel.repaint()
      // debug END
      cycle.updated(total, settled)
      if (total == settled) Updater.empty
      else new Updater(unsettledPoints, cycle.next)
    }

    val hasPoints: Boolean = true

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
     * Creates a new updater, that can start the update process for the provided points
     * @param points the points to update. This update must be the first for them in the actual calculation
     * @param cycle the actual iteration cycle
     */
    def start(points: Traversable[Point], cycle: IterationCycle)(implicit ec: ExecutionContext) = newUpdater(points, cycle, starter = true)

    /**
     * Creates a new updater, that can continue the update process for the provided points
     * @param points the points to update. They must have updated at least once in the actual calculation
     * @param cycle the actual iteration cycle
     */
    def continue(points: Traversable[Point], cycle: IterationCycle)(implicit ec: ExecutionContext) = newUpdater(points, cycle, starter = false)

    // TODO define a trait for updater and provide various implementations
    def empty = new Updater(null, null) {
      override def hasPoints = false
    }

    def completed(points: Traversable[Point], cycle: IterationCycle) = new Updater(points, cycle) {
      override def update() = Future.successful(this)
    }

    /**
     * Creates a new updater
     * @param points the points to update
     * @param cycle the actual iteration cycle
     * @param starter true, if this updater starts the calculation for the points
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




