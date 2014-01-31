package hu.nemzsom.mandelbrot

import scala.annotation.tailrec
import java.util.concurrent.{Executors, ThreadPoolExecutor}
import scala.concurrent._
import scala.math._
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger}
import javax.swing.Timer
import java.awt.event.{ActionEvent, ActionListener}

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
  val iterationStep = 30   // TODO dynamic?
  val maxDividable = 17
  val updaters = new AtomicInteger(0)
  val paintTimer = new Timer(100, new ActionListener {
    def actionPerformed(e: ActionEvent): Unit = painter.repaint()
  })
  paintTimer.setInitialDelay(300)
  paintTimer.start()

  val numOfProcs = Runtime.getRuntime.availableProcessors
  val executor: ThreadPoolExecutor = Executors.newFixedThreadPool(numOfProcs * 2).asInstanceOf[ThreadPoolExecutor]
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutor(executor)

  def calculate(): Unit = {
    debugTime = System.nanoTime
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
    updateStarted(8)
    calc(borders.map(Updater(_).update), Updater(mainArea.subArea(1, 1, width, height)), iterationStep)
  }

  private def calc(borders: IndexedSeq[Future[Updater]], middle: Updater, maxIter: Int): Unit = {
    Future.sequence(borders).onSuccess {
      case updatedBorders: IndexedSeq[Updater] =>
        if (checkAllUniform(updatedBorders, maxIter)) {
          // uniform borders
          //println(s"UNIFORM BORDERS for middle $middle and its borders (at $maxIter maxIter).")
          handleUniform(updatedBorders, middle, maxIter)
        }
        else if (math.max(middle.area.width, middle.area.height) < maxDividable) {
          // non uniform borders and area reached minimal size
          //println(s"RECURSIVE update for middle $middle and its borders. (at $maxIter maxIter)")
          updateStarted(1)
          recursiveUpdate(middle)
          updatedBorders.foreach(recursiveUpdate)
        }
        else {
          // non uniform borders and area can further divided
          //println(s"DIVIDE for middle $middle and its borders. (head: ${borders.head}}), (at $maxIter maxIter)")
          divideAndCalc(updatedBorders, middle, maxIter)
        }
    }
  }

  def checkAllUniform(borders: IndexedSeq[Updater], atIter: Int): Boolean = {
    val loc = getLocAt(borders.head.area.topLeft, atIter)
    borders.tail.map(_.area).forall { area =>
      area.forall ( getLocAt(_, atIter) == loc )
    }
  }

  def handleUniform(borders: IndexedSeq[Updater], middle: Updater, atIter: Int): Unit =
    if (atIter < globalMaxIter) {
      val loc = getLocAt(borders.head.area.topLeft, atIter)
      if (loc == Unsettled) calc(borders.map(_.update), middle, atIter + iterationStep)
      else { // Outside(iter) or Inside
        //println(s"MASS coloring for middle $middle.  (at $atIter atIter)")
        middle.area.foreach { p =>
          p.location = loc
          color(p)
        }
        updateEnded(8)
      }
    }
    else {
      (middle +: borders).map(_.area).foreach { area =>
        area.foreach(color)
      }
      updateEnded(8)
    }

  private def divideAndCalc(borders: IndexedSeq[Updater], middle: Updater, maxIter: Int) {
    updateStarted(8)
    val height = middle.area.height
    val width = middle.area.width
    if (height > width) { // horizontal cut
    val (mTop, mCut, mBottom) = middle.splitHorizontal
    val (lTop, lCut, lBottom) = borders(Left.id).splitHorizontal
    val (rTop, rCut, rBottom) = borders(Right.id).splitHorizontal
    Updater(mCut.area, maxIter).update.onSuccess { case mCutUpdated =>
      val topBorders = borders.take(3) ++ IndexedSeq(rTop, rCut, mCutUpdated, lCut, lTop)
      val bottomBorders = IndexedSeq(lCut, mCutUpdated, rCut, rBottom, borders(BottomRight.id), borders(Bottom.id), borders(BottomLeft.id), lBottom)
      calc(topBorders.map(b => Future.successful(b)), mTop, maxIter)
      calc(bottomBorders.map(b => Future.successful(b)), mBottom, maxIter)
      }
    }
    else { // vertical cut
      val (mLeft, mCut, mRight) = middle.splitVertical
      val (tLeft, tCut, tRight) = borders(Top.id).splitVertical
      val (bLeft, bCut, bRight) = borders(Bottom.id).splitVertical
      Updater(mCut.area, maxIter).update.onSuccess { case mCutUpdated =>
        val leftBorders = IndexedSeq(borders(TopLeft.id), tLeft, tCut, mCutUpdated, bCut, bLeft, borders(BottomLeft.id), borders(Left.id))
        val rightBorders = IndexedSeq(tCut, tRight, borders(TopRight.id), borders(Right.id), borders(BottomRight.id), bRight, bCut, mCutUpdated)
        calc(leftBorders.map(b => Future.successful(b)), mLeft, maxIter)
        calc(rightBorders.map(b => Future.successful(b)), mRight, maxIter)
      }
    }
  }

  private def getLocAt(point: Point, iter: Int): PointLoc = point.location match {
    case Outside(i) if (i > iter) => Unsettled
    case loc => loc
  }
  

  private def recursiveUpdate(updater: Updater): Unit =
    if (updater.maxIter < globalMaxIter && !updater.points.isEmpty) {
      //println("recurse")
      updater.update.onSuccess { case u => recursiveUpdate(u) }
    } else {
      updater.points.foreach(color)
      updateEnded(1)
    }

  private def updateStarted(amount: Int): Unit = updaters.addAndGet(amount)

  private def updateEnded(amount: Int): Unit =  {
    val processes = updaters.addAndGet(-amount)
    if (processes == 0) {
      painter.repaint()
      println(s"Done in ${(System.nanoTime - debugTime) / 1000000} ms")
      paintTimer.stop()
    }
  }

  class Updater private(val area: Area, val points: Traversable[Point], val maxIter: Int)(implicit ec: ExecutionContext) {

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
     * Updates one point to maxIter. The default implementation expects only [[hu.nemzsom.mandelbrot.Unsettled]] points
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
}

}




