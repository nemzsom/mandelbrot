package hu.nemzsom.mandelbrot

import scala.annotation.tailrec
import scala.concurrent._
import scala.math._
import rx.lang.scala.{Subscription, Observer, Observable}
import java.util.concurrent.atomic.AtomicInteger
import com.typesafe.scalalogging.slf4j.Logger
import org.slf4j.LoggerFactory
import scala.util.{Failure, Success}

case class CalcStat(total: Int, settled: Int, maxIter: Int)

/**
 * It calculates an area's points.
 */
class Calculator(mainArea: Area, plotter: Plotter)(implicit ec: ExecutionContext) {

  private object Config {
    val maxDividableSize = 14
    val iterationStep = 10
  }

  import Config._

  protected val logger: Logger = Logger(LoggerFactory getLogger "mandelbrot.Calculator")

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
      logger.trace(s"$this update with total: $total, settled: $settled.")
      // DEBUG END
      if (total > 0) {
        _count_settled.addAndGet(settled)
        endCheck(_remaining_total.addAndGet(-total))
      }
    }

    private def previousFinished(initiallySettled: Int): Unit = {
      // DEBUG
      logger.trace(s"$this prev finished with initiallySettled: $initiallySettled.")
      // DEBUG END
      if (initiallySettled != 0) {
        _initially_settled = initiallySettled
        if (initiallySettled < mainArea.size) // to prevent infinite recursion
          endCheck(_remaining_total.addAndGet(-initiallySettled))
      }
    }

    private def endCheck(remaining: Int): Unit = {
      // DEBUG
      logger.trace(s"Check with remaining: $remaining.")
      // DEBUG END
      assert(remaining >= 0, s"remaining: $remaining") // TODO remove after debug
      if (remaining == 0) {
        val settled = _count_settled.get
        observer.onNext(CalcStat(mainArea.size - _initially_settled, settled, maxIter))
        next.previousFinished(_initially_settled + settled)
      }
    }

    def updateProcessStopped(): Unit = {
      assert(updateProcessCount.get() > 0) // TODO remove after debug
      if (updateProcessCount.decrementAndGet() == 0) {
        observer.onCompleted()
      }
    }

    override def toString = s"cycle($maxIter)[remaining: ${_remaining_total}, settled: ${_count_settled}, initially settled: ${_initially_settled}]"
  }

  private val subscription: Subscription = new Subscription {}

  private val updateProcessCount = new AtomicInteger()

  def calculate(): Observable[CalcStat] = Observable.create(
    (observer: Observer[CalcStat]) => {
      // TODO divide initially to the number of processors
      // TODO if both size bigger than 4 on the complex plane, divide further (to defense against a uniform border entirely outside of the mandelbrot set)
      updateProcessCount.incrementAndGet()
      calcWithBorder(mainArea, Updater.start(mainArea.border, new IterationCycle(iterationStep, observer)))
      subscription
    }
  )

  private def calcWithBorder(area: Area, borderUpdater: Updater): Unit = {
    val cycle = borderUpdater.cycle
    if (!subscription.isUnsubscribed) {
      val border = borderUpdater.points
      borderUpdater.update().onComplete {
        case Success(borderNext) =>
          if (isUniform(border)) {
            handleUniformBorder(area, cycle, borderNext)
          }
          else if (isDividable(area)) {
            divideAndCalc(area, cycle)
          }
          else {
            Updater.start(inner(area), cycle).update().onComplete {
              case Success(innerNext) =>
                recurseCalc(Updater.merge(borderNext, innerNext))
              case Failure(t) => logger.error("Inner update failed", t)
            }
          }
        case Failure(t) => logger.error("Border update failed", t)
      }
    }
    else {
      cycle.updateProcessStopped()
    }
  }

  private def handleUniformBorder(area: Area, cycle: IterationCycle, borderNext: Updater): Unit = {
    val loc = area.topLeft.location
    // area size without borders
    val innerAreaSize = (area.width - 2) * (area.height - 2)
    if (loc == Unsettled) {
      // Unsettled border
      cycle.updated(innerAreaSize, 0)
      calcWithBorder(area, borderNext)
    }
    else {
      // Inside or outside (with same iter) border
      val innerArea = inner(area)
      innerArea.foreach{ point =>
        point.location = loc
        plotter.plot(point)
      }
      cycle.updated(innerAreaSize, innerAreaSize)
      cycle.updateProcessStopped()
    }
  }

  private def divideAndCalc(area: Area, cycle: IterationCycle): Unit = {
      def calc(a1: Area, a2: Area, dividingLine: Area): Unit = {
        Updater.start(dividingLine, cycle).update().onComplete {
          case Success(_) =>
            updateProcessCount.incrementAndGet()
            calcWithBorder(a1, Updater.completed(a1.border, cycle))
            calcWithBorder(a2, Updater.completed(a2.border, cycle))
          case Failure(t) => logger.error("Divide line update failed", t)
        }
      }
    if (area.width > area.height) {
      val (left, right) = area.splitVertical
      val dividingLine = area.subArea(left.width - 1, 1, 2, area.height - 2)
      calc(left, right, dividingLine)
    }
    else {
      val (top, bottom) = area.splitHorizontal
      val dividingLine = area.subArea(1, top.height - 1, area.width - 2, 2)
      calc(top, bottom, dividingLine)
    }
  }

  private def recurseCalc(updater: Updater): Unit =
    if (!subscription.isUnsubscribed && updater.hasPoints) {
      updater.update().onComplete {
        case Success(nextUpdater) =>
          recurseCalc(nextUpdater)
        case Failure(t) => logger.error("Recurse update failed", t)
      }
    }
    else {
      updater.cycle.updateProcessStopped()
    }

  private def isDividable(area : Area): Boolean =
    area.width >= maxDividableSize || area.height >= maxDividableSize

  private def isUniform(points: Traversable[Point]): Boolean =
    points.forall(_.location == points.head.location)

  /** Inner area without borders */
  private def inner(area: Area) = area.subArea(1, 1, area.width - 2, area.height - 2)

  trait Updater {

    val hasPoints: Boolean
    val points: Traversable[Point]
    val cycle: IterationCycle

    lazy val maxIter = cycle.maxIter

    def update(): Future[Updater]
  }

  object Updater {

    /**
     * Creates a new updater, that can start the update process for the provided points
     * @param points the points to update. This update must be the first for them in the actual calculation
     * @param cycle the actual iteration cycle
     */
    def start(points: Traversable[Point], cycle: IterationCycle) = new StartUpdater(points, cycle, firstStart = true)

    /**
     * Creates a new updater, that can continue the update process for the provided points
     * @param points the points to update. They must have updated at least once in the actual calculation
     * @param cycle the actual iteration cycle
     */
    def continue(points: Traversable[Point], cycle: IterationCycle) = new StartUpdater(points, cycle, firstStart = false)

    def merge(updaterA: Updater, updaterB: Updater) = {
      //require(updaterA.cycle == updaterB.cycle, s"updaterA cycle: ${updaterA.cycle}, updaterB cycle: ${updaterB.cycle}")
      if (updaterA.hasPoints || updaterB.hasPoints) continue(updaterA.points ++ updaterB.points, updaterA.cycle)
      else empty(updaterA.cycle)
    }

    def empty(cycle: IterationCycle) = new EmptyUpdater(cycle)

    def completed(points: Traversable[Point], cycle: IterationCycle) = new CompletedUpdater(points, cycle)

    class CalcUpdater(val points: Traversable[Point], val cycle: IterationCycle) extends Updater {

      val hasPoints = true

      def update(): Future[Updater] = future {
        // debug BEGIN
//              logger.trace(s"UPDATE at $cycle")
//              points.foreach ( point => debugPixels(point.index) = 255 << 16 )
//              debugPanel.repaint()
//              debugQuene.take
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
//              points foreach plotter.plot
//              logger.trace(s"DONE at $cycle. total: $total, settled: $settled")
//              debugPanel.repaint()
        // debug END
        cycle.updated(total, settled)
        if (total == settled) empty(cycle.next)
        else new CalcUpdater(unsettledPoints, cycle.next)
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

    class StartUpdater(points: Traversable[Point], cycle: IterationCycle, firstStart: Boolean) extends CalcUpdater(points, cycle) {

      // It handles any Point
      override def updatePoint(point: Point): Boolean = {
        if (point.location == Unsettled) {
          if (point.iter == 0 && preCheck_isInside(point.complexValue)) {
            point.location = Inside
            true
          }
          else super.updatePoint(point)
        }
        else firstStart
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

    class EmptyUpdater(val cycle: IterationCycle) extends Updater {

      val hasPoints = false
      val points = Traversable()

      override def update(): Nothing = throw new RuntimeException("update on empty updater") // TODO proper exception
    }

    class CompletedUpdater(val points: Traversable[Point], val cycle: IterationCycle) extends Updater {

      val hasPoints = true

      override def update() = Future.successful(continue(points, cycle.next))
    }
  }

}




