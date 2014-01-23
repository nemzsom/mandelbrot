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

  import BorderPos._

  val mainArea: Area
  val globalMaxIter = 300 // TODO dynamic
  val iterationStep = 10   // TODO dynamic?
  val maxSideSize = 10

  val numOfProcs = Runtime.getRuntime.availableProcessors
  val executor: ThreadPoolExecutor = Executors.newFixedThreadPool(numOfProcs * 2).asInstanceOf[ThreadPoolExecutor]
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutor(executor)

  def calculate(): Unit = {
    val width = mainArea.width - 2
    val height = mainArea.height - 2
    val borders = IndexedSeq(
      mainArea.subArea(0, 0, 1, 1),
      mainArea.subArea(1, 0, width, 1),
      mainArea.subArea(width + 1, 0, 1, 1),
      mainArea.subArea(width + 1, 1, 1, height),
      mainArea.subArea(width + 1, height + 1, 1, 1),
      mainArea.subArea(1, height + 1, width, 1),
      mainArea.subArea(0, height + 1, 1, 1),
      mainArea.subArea(0, 1, 1, height)
    )
    calc(borders.map(Updater(_)), Updater(mainArea.subArea(1, 1, width, height)))
  }

  private def calc(borders: IndexedSeq[Updater], middle: Updater): Unit = {
    val updates = borders.map(_.update)
    Future.sequence(updates).onSuccess {
      case updatedBorders: IndexedSeq[Updater] =>
        /*if (updatedBorders.forall(_.area.isUniform)) {
          // uniform borders
          // TODO implement
        }
        else*/ if (middle.area.width < maxSideSize || middle.area.height < maxSideSize) {
          // non uniform borders and area reached minimal size
          recursiveUpdate(middle)
          updatedBorders.foreach(recursiveUpdate)
        }
        else {
          // non uniform borders and area can further divided
          divideAndCalc(updatedBorders, middle)
        }
    }
  }

  private def divideAndCalc(borders: IndexedSeq[Updater], middle: Updater) {
    val height = middle.area.height
    val width = middle.area.width
    if (height > width) { // horizontal cut
      val (mTop, mCut, mBottom) = middle.splitHorizontal
      val (lTop, lCut, lBottom) = borders(Left.id).splitHorizontal
      val (rTop, rCut, rBottom) = borders(Right.id).splitHorizontal
      mCut.update.onSuccess { case mCutUpdated =>
        val topBorders = borders.take(3) ++ IndexedSeq(rTop, rCut, mCutUpdated, lCut, lTop)
        val bottomBorders = IndexedSeq(lCut, mCutUpdated, rCut, rBottom, borders(BottomRight.id), borders(Bottom.id), borders(BottomLeft.id), lBottom)
        calc(topBorders, mTop)
        calc(bottomBorders, mBottom)
      }
    }
    else { // vertical cut
      val (mLeft, mCut, mRight) = middle.splitVertical
      val (tLeft, tCut, tRight) = borders(Top.id).splitVertical
      val (bLeft, bCut, bRight) = borders(Bottom.id).splitVertical
      mCut.update.onSuccess { case mCutUpdated =>
        val leftBorders = IndexedSeq(borders(TopLeft.id), tLeft, tCut, mCutUpdated, bCut, bLeft, borders(BottomLeft.id), borders(Left.id))
        val rightBorders = IndexedSeq(tCut, tRight, borders(TopRight.id), borders(Right.id), borders(BottomRight.id), bRight, bCut, mCutUpdated)
        calc(leftBorders, mLeft)
        calc(rightBorders, mRight)
      }
    }
  }
  

  private def recursiveUpdate(updater: Updater): Unit =  if (updater.maxIter < globalMaxIter && !updater.points.isEmpty) {
    updater.update.onSuccess { case u => recursiveUpdate(u) }
  }

  class Updater private(val area: Area, val points: Traversable[Point], val maxIter: Int)(implicit ec: ExecutionContext) {

    lazy val update: Future[Updater] = future {
      //println(s"Points to update at $maxIter: ${points.size}")

      area.foreach( point => pixels(point.index) = 255 << 16)
      painter.repaint()
      debugQuene.take
      val unsettledPoints = updateAll()
      painter.repaint() // TODO repaint only after a whole update cycle
      new Updater(area, unsettledPoints, maxIter + iterationStep)
    }

    def splitVertical: (Updater, Updater, Updater) = {
      val (leftArea, cutArea, rightArea) = area.splitVertical
      (
        Updater(leftArea, maxIter),
        Updater(cutArea, maxIter),
        Updater(rightArea, maxIter)
        )
    }

    def splitHorizontal: (Updater, Updater, Updater) = {
      val (topArea, cutArea, bottomArea) = area.splitHorizontal
      (
        Updater(topArea, maxIter),
        Updater(cutArea, maxIter),
        Updater(bottomArea, maxIter)
        )
    }

    private def updateAll(): Traversable[Point] = {
      var unsettledPoints = List.empty[Point]
      points.foreach {
        point =>
          updatePoint(point)
          if (point.location == Unsettled)
            unsettledPoints = point :: unsettledPoints
          else
            color(point)
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
}

}




