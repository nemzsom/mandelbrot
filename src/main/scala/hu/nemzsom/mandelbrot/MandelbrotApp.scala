package hu.nemzsom.mandelbrot

import scala.swing.Swing._
import scala.swing.{MainFrame, Panel, SimpleSwingApplication}
import java.awt.Graphics2D
import scala.swing.event._
import scala.swing.event.MousePressed
import scala.swing.event.MouseWheelMoved
import scala.swing.event.MouseDragged
import scala.swing.event.UIElementResized
import java.awt.image.BufferedImage
import java.util.concurrent.{Executors, ThreadPoolExecutor}
import scala.concurrent.ExecutionContext
import rx.lang.scala.Subscription
import com.typesafe.scalalogging.slf4j.Logger
import org.slf4j.LoggerFactory

object MandelbrotApp extends SimpleSwingApplication {

  val numOfProcs = Runtime.getRuntime.availableProcessors
  val executor: ThreadPoolExecutor = Executors.newFixedThreadPool(numOfProcs * 2).asInstanceOf[ThreadPoolExecutor]
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutor(executor)

  protected val logger: Logger = Logger(LoggerFactory getLogger "mandelbrot.App")

  val width = 1000
  val height = 1000
  val mainArea = Area(Complex(-2, -2), 4, width, height)
  debugPanel = ui
  setDebugArea(mainArea)

  var image = new BufferedImage(mainArea.width, mainArea.height, BufferedImage.TYPE_INT_RGB)

  val plotter = new BImagePlotter(image, new Black_and_WhiteColorMap)
  val calculator = new Calculator(mainArea, plotter)

  debugTime = System.nanoTime
  val subscription: Subscription = calculator.calculate().subscribe( stat => stat match {
    case CalcStat(total, settled, maxIter) =>
      logger.debug(s"NEXT at maxIter $maxIter total: $total, settled: $settled (after ${(System.nanoTime - debugTime) / 1000000} ms)")
      if (maxIter > 300) {
        subscription.unsubscribe()
        logger.info(s"CALC_DONE ${(System.nanoTime - debugTime) / 1000000} ms")
      }
      ui.repaint()
  })

  lazy val ui = new Panel {

    preferredSize = (mainArea.width, mainArea.height)
    focusable = true
    listenTo(this, mouse.moves, mouse.clicks, mouse.wheel)

    reactions += {
      case e: MousePressed => /*println(s"clicked: ${e.point}");*/ debugQuene.put(0)
      case e: MouseDragged =>
      case e: MouseWheelMoved => (1 to 10).foreach(debugQuene.put(_))
      case _: UIElementResized =>
      case _: FocusLost => repaint()
    }

    override def paintComponent(g: Graphics2D) = {
      super.paintComponent(g)
      g.drawImage(image, 0, 0, image.getWidth, image.getHeight, null)
    }
  }

  def top = new MainFrame {
    title = "Mandelbrot set"
    contents = ui
  }
}
