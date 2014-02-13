package hu.nemzsom.mandelbrot

import scala.swing.{MainFrame, SimpleSwingApplication}
import com.typesafe.scalalogging.slf4j.Logger
import org.slf4j.LoggerFactory
import scala.swing.event.{MouseWheelMoved, MouseDragged, MousePressed}

object MandelbrotApp extends SimpleSwingApplication {

  protected val logger: Logger = Logger(LoggerFactory getLogger "mandelbrot.App")

  val width = 300
  val height = 300

  val panel = new ImagePanel(width, height)

  // DEBUG
  debugPanel = panel
  // DEBUG END

  val controller = new Controller(panel)

  // DEBUG
  panel.reactions += {
    case e: MousePressed => /*println(s"clicked: ${e.point}");*/ debugQuene.put(0)
    case e: MouseDragged =>
    case e: MouseWheelMoved => (1 to 10).foreach(debugQuene.put(_))
  }
  // DEBUG END

  def top = new MainFrame {
    title = "Mandelbrot set"
    contents = panel
  }
}
