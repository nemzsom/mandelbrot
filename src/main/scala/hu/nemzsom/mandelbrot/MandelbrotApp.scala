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
    new ColorMap.Grayscale(_) /*with SmoothColorMap*/,
    new ColorMap.BlueYellow(_) with SmoothColorMap,
    new ColorMap.Keki(_) with SmoothColorMap,
    new ColorMap.Keki2(_) with SmoothColorMap
  )

  val panel = new ImagePanel(width, height)

  val controller = new Controller(panel, colorMaps)

  // DEBUG
  panel.reactions += {
    case e: MousePressed =>
      println(s"clicked: ${e.point}")
      val c = controller.calculation.area.pointAt(e.point.x, e.point.y).complexValue
      val precision = c match {
        case c_BigDec: ComplexWithBigDecimal => c_BigDec.re_asBigDec.mc.getPrecision
        case c_Double: ComplexWithDouble => "double"
      }
      val scale = c match {
        case c_BigDec: ComplexWithBigDecimal => c_BigDec.re_asBigDec.scale
        case c_Double: ComplexWithDouble => "double"
      }
      println(s"clicked point's actual complex precision: $precision, scale: $scale")
    case KeyPressed(_, Key.Space, _, _) => debugQuene.put(0)
    case KeyPressed(_, Key.I, _, _) =>
      println("--------------- INFO -----------------")
      println(controller.calculation.area)
      println(s"colorIndex: ${controller.colorIndex}, colorCount: ${controller.colorCount}")
      val tlC = controller.calculation.area.topLeft.complexValue
      val precision = tlC match {
        case c_BigDec: ComplexWithBigDecimal => c_BigDec.re_asBigDec.mc.getPrecision
        case c_Double: ComplexWithDouble => "double"
      }
      val scale = tlC match {
        case c_BigDec: ComplexWithBigDecimal => c_BigDec.re_asBigDec.scale
        case c_Double: ComplexWithDouble => "double"
      }
      println(s"actual complex precision: $precision, scale: $scale")
      println(s"actual scale: ${controller.calculation.area.scale}")

      val scaleAsDouble = controller.calculation.area.scale.asDouble
      println(s"actual SCALE's exponent: ${java.lang.Math.getExponent(scaleAsDouble)}")
      val iPart = scaleAsDouble.toLong;
      val fPart = scaleAsDouble - iPart;
      println("Integer part = " + iPart);
      println("Fractional part = " + fPart);
      println("--------------------------------------")
  }
  // DEBUG END

  def top = new MainFrame {
    title = "Mandelbrot set"
    contents = panel
  }
}
