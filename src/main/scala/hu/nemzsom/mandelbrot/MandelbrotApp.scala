package hu.nemzsom.mandelbrot

import scala.swing.{MainFrame, SimpleSwingApplication}
import com.typesafe.scalalogging.slf4j.Logger
import org.slf4j.LoggerFactory
import scala.swing.event._
import scala.swing.event.KeyPressed
import scala.swing.event.MousePressed

object MandelbrotApp extends SimpleSwingApplication {

  protected val logger: Logger = Logger(LoggerFactory getLogger "mandelbrot.App")

  val width = 640
  val height = 480
  val colorMap = new ColorMap.Brown(60) with SmoothColorMap

  val panel = new ImagePanel(width, height)

  val controller = new Controller(panel, colorMap)

  // DEBUG
  panel.reactions += {
    case e: MousePressed => println(s"clicked: ${e.point}")
    case KeyPressed(_, Key.Space, _, _) => debugQuene.put(0)
  }
  // DEBUG END

  def top = new MainFrame {
    title = "Mandelbrot set"
    contents = panel
  }
}
