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
  val colorMaps: Array[Int => ColorMap] = Array(
    new ColorMap.Sunset(_) with SmoothColorMap,
    new ColorMap.Brown(_) with SmoothColorMap,
    new ColorMap.Grayscale(_) with SmoothColorMap,
    new ColorMap.BlueYellow(_) with SmoothColorMap,
    new ColorMap.Keki(_) with SmoothColorMap,
    new ColorMap.Keki2(_) with SmoothColorMap
  )

  val panel = new ImagePanel(width, height)

  val controller = new Controller(panel, colorMaps)

  // DEBUG
  panel.reactions += {
    case e: MousePressed =>
      val p = e.point
      println(s"clicked: $p, ${controller.calculation.area.pointAt(p.x, p.y)}")

    case KeyPressed(_, Key.Space, _, _) => debugQuene.put(0)
    case KeyPressed(_, Key.I, _, _) =>
      println("--------------- INFO -----------------")
      println(controller.calculation.area)
      println(s"colorIndex: ${controller.colorIndex}, colorCount: ${controller.colorCount}")
      println("--------------------------------------")
  }
  // DEBUG END

  def top = new MainFrame {
    title = "Mandelbrot set"
    contents = panel
  }
}
