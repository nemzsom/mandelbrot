package hu.nemzsom.mandelbrot

import rx.lang.scala.Subscription
import com.typesafe.scalalogging.slf4j.Logger
import org.slf4j.LoggerFactory
import java.util.concurrent.{Executors, ThreadPoolExecutor}
import scala.concurrent.ExecutionContext
import rx.subjects.BehaviorSubject

class Controller(panel: ImagePanel) {

  val numOfProcs = Runtime.getRuntime.availableProcessors
  val executor: ThreadPoolExecutor = Executors.newFixedThreadPool(numOfProcs * 2).asInstanceOf[ThreadPoolExecutor]
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutor(executor)

  protected val logger: Logger = Logger(LoggerFactory getLogger "mandelbrot.Controller")

  val plotter = new BImagePlotter(panel.image, new Black_and_WhiteColorMap)
  val area = Area(Complex(-2, -2), 4, panel.image.getWidth, panel.image.getHeight)
  val calculator = new Calculator(area, plotter)

  debugTime = System.nanoTime

  val subscription: Subscription = calculator.calculate().subscribe(
    stat => stat match {
      case CalcStat(total, settled, maxIter) =>
        logger.debug(s"NEXT at maxIter $maxIter total: $total, settled: $settled (after ${(System.nanoTime - debugTime) / 1000000} ms)")
        if (maxIter > 300) {
          subscription.unsubscribe()
        }
        panel.repaint()
    },
    error => logger.error(s"Error happened $error"),
    () => {
      logger.info(s"CALC_DONE ${(System.nanoTime - debugTime) / 1000000} ms")
      panel.repaint()
    }
  )

  panel.resized.subscribe { dimension =>
    if (area.width != dimension.width || area.height != dimension.height) {
      subscription.unsubscribe()
      // resize area: area = area.resize(dimension.width, dimension.height)
      // register callback to calc done: start new calculation with new area, new bufferedImage
    }
  }

  /*panel.resized.subscribe( dim => logger.debug(s"resize $dim"))
  panel.mouseDragged.subscribe( xy => xy match {
    case (x, y) => logger.debug(s"dragged x: $x, y: $y")
  })
  panel.mouseWheelMoved.subscribe( e => logger.debug(s"wheelMoved $e"))*/

}
