package hu.nemzsom.mandelbrot

import rx.lang.scala.Subscription
import com.typesafe.scalalogging.slf4j.Logger
import org.slf4j.LoggerFactory
import java.util.concurrent.{Executors, ThreadPoolExecutor}
import scala.concurrent.ExecutionContext
import scala.swing.Swing
import scala.collection.immutable.Queue
import java.util.concurrent.atomic.{AtomicInteger, AtomicBoolean}

sealed trait UIRequest
case class Resize(width: Int, height: Int) extends UIRequest
case class Drag(diffX: Int, diffY: Int) extends UIRequest
case class Zoom(rotation: Int, at: (Int, Int)) extends UIRequest

class Controller(panel: ImagePanel) {

  val numOfProcs = Runtime.getRuntime.availableProcessors
  val executor: ThreadPoolExecutor = Executors.newFixedThreadPool(numOfProcs * 2).asInstanceOf[ThreadPoolExecutor]
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutor(executor)

  protected val logger: Logger = Logger(LoggerFactory getLogger "mandelbrot.Controller")

  var requests = Queue.empty[UIRequest]
  var cleanNeeded = new AtomicBoolean(false)
  var calculation = startNewCalculation(Area(Complex(-2, -2), 4, panel.image.getWidth, panel.image.getHeight))

  panel.resized.subscribe { dimension =>
    logger.debug(s"panel resized to $dimension")
    val resizes = requests.filter(_.isInstanceOf[Resize]).asInstanceOf[Queue[Resize]]
    val (currWidth, currHeight) =
      if (resizes.isEmpty) (calculation.area.width, calculation.area.height)
      else (resizes.last.width, resizes.last.height)
    if (currWidth != dimension.width || currHeight != dimension.height) {
      onRequest(Resize(dimension.width, dimension.height))
    }
  }

  panel.mouseDragged.subscribe( xy => xy match {
    case (x, y) => onRequest(Drag(x, y))
  })

  panel.mouseWheelMoved.subscribe { e =>
    val point = e.point
    onRequest(Zoom(e.rotation, (point.x, point.y)))
  }

  def startNewCalculation(area: Area): Calculation =
    new Calculation(area,  new BImagePlotter(panel.image, new LinearColorMap(450, ColorMap.fromColors((237, 1f, 0.35f), (40, 1f, 1f)))))

  def onRequest(req: UIRequest): Unit = {
    requests = requests.enqueue(req)
    if (calculation.running) calculation.stop()
    else processRequests()
  }

  def processRequests(): Unit = {
    var area = calculation.area
    logger.debug(s"processRequests $requests")
    requests.foreach{
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
    }
    requests = Queue.empty
    panel.repaint()
    calculation = startNewCalculation(area)
  }

  /**
   * It represents a calculation instance
   */
  class Calculation(val area: Area, val plotter: Plotter) {

    var running = true
    val calculator = new Calculator(area, plotter)

    val startToSettle = new AtomicBoolean(false)

    debugTime = System.nanoTime

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
          panel.repaint()
      },
      error => logger.error("Error happened", error),
      () => Swing.onEDT {
        logger.info(s"CALC_DONE ${(System.nanoTime - debugTime) / 1000000} ms")
        running = false
        if (!requests.isEmpty) processRequests()
        else {
          plotter.finish(area)
          panel.repaint()
        }
      }
    )

    def stop(): Unit = subscription.unsubscribe()
  }

}
