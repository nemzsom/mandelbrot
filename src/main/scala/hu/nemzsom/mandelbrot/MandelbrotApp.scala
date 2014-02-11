package hu.nemzsom.mandelbrot

import scala.swing.{MainFrame, SimpleSwingApplication}
import com.typesafe.scalalogging.slf4j.Logger
import org.slf4j.LoggerFactory

object MandelbrotApp extends SimpleSwingApplication {

  protected val logger: Logger = Logger(LoggerFactory getLogger "mandelbrot.App")

  val width = 200
  val height = 200

  val panel = new ImagePanel(width, height)
  val controller = new Controller(panel)

  def top = new MainFrame {
    title = "Mandelbrot set"
    contents = panel
  }
}
