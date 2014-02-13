package hu.nemzsom.mandelbrot

import rx.lang.scala.Subscription
import com.typesafe.scalalogging.slf4j.Logger
import org.slf4j.LoggerFactory
import java.util.concurrent.{Executors, ThreadPoolExecutor}
import scala.concurrent.ExecutionContext
import rx.subjects.BehaviorSubject
import java.awt.EventQueue
import scala.swing.Swing
import scala.collection.immutable.Queue

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
  var calculation = startNewCalculation(Area(Complex(-2, -2), 4, panel.image.getWidth, panel.image.getHeight))

  panel.resized.subscribe { dimension =>
    if (calculation.area.width != dimension.width || calculation.area.height != dimension.height) {
      logger.debug(s"panel resized to $dimension")
      //subscription.unsubscribe()
      // resize area: area = area.resize(dimension.width, dimension.height)
      // register callback to calc done: start new calculation with new area, new bufferedImage
    }
  }

  def startNewCalculation(area: Area): Calculation =
    new Calculation(area,  new BImagePlotter(panel.image, new Black_and_WhiteColorMap))

  /*panel.resized.subscribe( dim => logger.debug(s"resize $dim"))
  panel.mouseDragged.subscribe( xy => xy match {
    case (x, y) => logger.debug(s"dragged x: $x, y: $y")
  })
  panel.mouseWheelMoved.subscribe( e => logger.debug(s"wheelMoved $e"))*/

  def runqueue: Unit = {
    // TODO implement
    // process requests on the queue
    // start new calculation
  }

  /**
   * It represents a calculation instance
   */
  class Calculation(val area: Area, val plotter: Plotter) {

    var running = true
    val calculator = new Calculator(area, plotter)

    debugTime = System.nanoTime

    val subscription: Subscription = calculator.calculate().subscribe(
      stat => stat match {
        case CalcStat(total, settled, maxIter) =>
          logger.debug(s"NEXT at maxIter $maxIter total: $total, settled: $settled (after ${(System.nanoTime - debugTime) / 1000000} ms)")
          if (maxIter >= 300) { // TODO dynamically calculate the maximum iteration
            subscription.unsubscribe()
          }
          panel.repaint()
      },
      error => logger.error(s"Error happened $error"),
      () => Swing.onEDT {
        logger.info(s"CALC_DONE ${(System.nanoTime - debugTime) / 1000000} ms")
        running = false
        if (!requests.isEmpty) runqueue
        panel.repaint()
      }
    )
  }

}
