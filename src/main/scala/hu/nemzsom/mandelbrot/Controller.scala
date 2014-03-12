package hu.nemzsom.mandelbrot

import rx.lang.scala.Subscription
import com.typesafe.scalalogging.slf4j.Logger
import org.slf4j.LoggerFactory
import java.util.concurrent.{Executors, ThreadPoolExecutor}
import scala.concurrent.ExecutionContext
import scala.swing.Swing
import scala.collection.immutable.Queue
import java.util.concurrent.atomic.AtomicBoolean
import javax.swing.Timer
import java.awt.event.{InputEvent, ActionEvent, ActionListener}
import scala.util.{Success, Failure}
import scala.swing.event.{Key, KeyPressed}
import java.math.MathContext

sealed trait UIRequest

case class Resize(width: Int, height: Int) extends UIRequest

case class Drag(diffX: Int, diffY: Int) extends UIRequest

case class Zoom(rotation: Int, at: (Int, Int)) extends UIRequest

case class ChangeColor(indexDiff: Int, colorCountDiff: Int) extends UIRequest

case class TMPChangeNumFormat(precDiff: Int) extends UIRequest

class Controller(panel: ImagePanel, val colorMaps: Array[Int => ColorMap]) {

  val numOfProcs = Runtime.getRuntime.availableProcessors
  val executor: ThreadPoolExecutor = Executors.newFixedThreadPool(numOfProcs).asInstanceOf[ThreadPoolExecutor]
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutor(executor)

  protected val logger: Logger = Logger(LoggerFactory getLogger "mandelbrot.Controller")

  var colorIndex = 0
  var colorCount = 80
  var colorMap = colorMaps(colorIndex)(colorCount)
  var requests = Queue.empty[UIRequest]
  var cleanNeeded = new AtomicBoolean(false)
  val repaintTimer = new Timer(1000 / 10, new ActionListener {
    override def actionPerformed(e: ActionEvent): Unit = {
      panel.repaint()
    }
  })

  //var calculation = startNewCalculation(Area(Complex(BigDecimal(-2), BigDecimal(-1.65)), Scale(2.5 / (panel.image.getHeight - 1)), panel.image.getWidth, panel.image.getHeight))
  var calculation = startNewCalculation(Area(Complex(-2, -1.65), Scale(2.5 / (panel.image.getHeight - 1)), panel.image.getWidth, panel.image.getHeight))


  panel.resized.subscribe {
    dimension =>
      logger.debug(s"panel resized to $dimension")
      val resizes = requests.filter(_.isInstanceOf[Resize]).asInstanceOf[Queue[Resize]]
      val (currWidth, currHeight) =
        if (resizes.isEmpty) (calculation.area.width, calculation.area.height)
        else (resizes.last.width, resizes.last.height)
      if (currWidth != dimension.width || currHeight != dimension.height) {
        onRequest(Resize(dimension.width, dimension.height))
      }
  }

  panel.mouseDragged.subscribe(xy => xy match {
    case (x, y) => onRequest(Drag(x, y))
  })

  panel.mouseWheelMoved.subscribe {
    e =>
      val point = e.point
      onRequest(Zoom(e.rotation, (point.x, point.y)))
  }

  panel.keyPressed.subscribe(e => e match {
    case KeyPressed(_, Key.C, modifiers, _) =>
      if ((modifiers & InputEvent.CTRL_DOWN_MASK) != 0) {
        onRequest(ChangeColor(0, 10))
      }
      else if ((modifiers & InputEvent.SHIFT_DOWN_MASK) != 0) {
        onRequest(ChangeColor(0, -10))
      }
      else {
        onRequest(ChangeColor(1, 0))
      }
    case KeyPressed(_, Key.N, modifiers, _) =>
      if ((modifiers & InputEvent.CTRL_DOWN_MASK) != 0) {
        onRequest(TMPChangeNumFormat(-2))
      }
      else {
        onRequest(TMPChangeNumFormat(2))
      }
    case _ =>
  })

  def startNewCalculation(area: Area): Calculation =
    new MandelCalc(area, new BImagePlotter(panel.image, colorMap))

  def onRequest(req: UIRequest): Unit = {
    requests = requests.enqueue(req)
    if (calculation.running) calculation.stop()
    else processRequests()
  }

  def processRequests(): Unit = {
    // TODO request processing should not block the Event Thread for long time
    // A: aggregate subsequent requests with same type in one request
    // B: process area and panel changes in an other thread with the possibility of interruption (maybe using a calculation trait or similar)
    var debugTime = System.nanoTime
    var area = calculation.area
    logger.debug(s"processRequests $requests")
    requests.foreach {
      case Resize(width, height) =>
        logger.debug(s"process resize for ($width, $height)")
        area = area.resize(width, height)
        panel.resizeImage(width, height)
      case Drag(diffX, diffY) =>
        logger.debug(s"process drag for ($diffX, $diffY)")
        area = area.move(diffX, diffY)
        panel.moveImage(diffX, diffY)
      case Zoom(rotation, at) =>
        logger.debug(s"process zoom for rotation $rotation at $at")
        val factor = Math.pow(1.25, -rotation)
        area = area.zoom(factor, at)
        panel.zoomImage(factor, at)
        cleanNeeded.set(true)
      case ChangeColor(indexDiff, colorCountDiff) =>
        logger.debug(s"process changeColor for indexDiff $indexDiff and colorCountDiff $colorCountDiff")
        colorIndex = {
          val newIndex = colorIndex + indexDiff
          if (newIndex >= colorMaps.size) 0
          else newIndex
        }
        colorCount = {
          val newColorCount = colorCount + colorCountDiff
          Math.max(2, newColorCount)
        }
        colorMap = colorMaps(colorIndex)(colorCount)
      case TMPChangeNumFormat(precDiff) =>
        val tlC = area.topLeft.complexValue
        val newPrec = (tlC match {
          case _: ComplexWithDouble => 15
          case cBD: ComplexWithBigDecimal => cBD.re_asBigDec.mc.getPrecision
        }) + precDiff
        val newTlC = Complex(tlC.re_asBigDec(new MathContext(newPrec)), tlC.im_asBigDec(new MathContext(newPrec)))
        area = Area(newTlC, area.scale, area.width, area.height)
    }
    debugTime = (System.nanoTime - debugTime) / 1000000
    if (debugTime > 200) {
      println(s"process requests done in $debugTime ms. Queue: $requests")
    }
    requests = Queue.empty
    panel.repaint()
    calculation = startNewCalculation(area)
  }

  trait Calculation {
    val area: Area

    def running: Boolean

    def stop(): Unit
  }

  /**
   * It represents a calculation instance
   */
  class MandelCalc(val area: Area, plotter: Plotter) extends Calculation {

    var running = true
    val calculator = new Calculator(area, plotter)

    val startToSettle = new AtomicBoolean(false)

    debugTime = System.nanoTime

    repaintTimer.start()

    val subscription: Subscription = calculator.calculate().subscribe(
      stat => stat match {
        case CalcStat(total, settled, maxIter) =>
          logger.debug(s"NEXT at maxIter $maxIter total: $total, settled: $settled (after ${(System.nanoTime - debugTime) / 1000000} ms)")
          if (startToSettle.get) {
            if (settled < 10 && cleanNeeded.getAndSet(false))
              area.filter(_.location == Unsettled) foreach plotter.plot // clean
            if (settled == 0 && subscription != null)
              subscription.unsubscribe()
          }
          else if (maxIter > calculator.Config.iterationStep && settled > 0) {
            startToSettle.set(true)
          }
      },
      error => logger.error("Error happened", error),
      () => Swing.onEDT {
        logger.info(s"CALC_DONE ${(System.nanoTime - debugTime) / 1000000} ms")
        running = false
        repaintTimer.stop()
        if (!requests.isEmpty) processRequests()
        else calculation = new FinalizerCalc(area, plotter)
      }
    )

    def stop(): Unit = subscription.unsubscribe()
  }

  class FinalizerCalc(val area: Area, plotter: Plotter) extends Calculation {

    import scala.concurrent._

    @volatile var running = true
    @volatile var cancel = false

    val f: Future[Unit] = future {
      plotter.finish(area, () => cancel)
      if (!cancel) panel.repaint()
    }

    f.onComplete {
      case Success(_) => Swing.onEDT {
        running = false
        if (!requests.isEmpty) processRequests()
      }
      case Failure(t) => logger.error("Finalizer failed", t)
    }

    override def stop(): Unit = cancel = true
  }

}
