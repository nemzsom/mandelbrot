package hu.nemzsom.mandelbrot.sandbox

import scala.swing.Swing._
import scala.swing.{MainFrame, Panel, SimpleSwingApplication}
import java.awt.image.{DataBufferInt, BufferedImage}
import java.awt.{Color, Graphics2D}
import scala.swing.event.{MouseWheelMoved, MousePressed}
import java.awt.event.InputEvent

object ColorMapSb extends SimpleSwingApplication {

  val width = 600
  val height = 100


  lazy val ui = new Panel {

    preferredSize = (width, height)
    var bufferedImage = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
    listenTo(mouse.clicks, mouse.wheel)

    reactions += {
      case MousePressed(_, _, modifiers, _, _) =>
        if ((modifiers & InputEvent.BUTTON1_DOWN_MASK) > 0) saturation = Math.min(saturation + 0.05f, 1.0f)
        if ((modifiers & InputEvent.BUTTON3_DOWN_MASK) > 0) saturation = Math.max(saturation - 0.05f, 0.0f)
        println(s"saturation: $saturation")
        updateColors()
        repaint()
      case MouseWheelMoved(_, _, _, rotation) =>
        if (rotation > 0) brightness = Math.min(brightness + 0.05f, 1.0f)
        if (rotation < 0) brightness = Math.max(brightness - 0.05f, 0.0f)
        println(s"brightness: $brightness")
        updateColors()
        repaint()
    }

    override def paintComponent(g: Graphics2D) = {
      super.paintComponent(g)
      for {
        x <- 0 until width
        color = colors(x % colorCount)
        y <- 0 until height
      } {
        bufferedImage.setRGB(x, y, color)
      }
      g.drawImage(bufferedImage, 0, 0, bufferedImage.getWidth, bufferedImage.getHeight, null)
    }
  }

  var saturation = 0.7f
  var brightness = 0.9f
  val colorCount = 300

  val colors = new Array[Int](colorCount)

  def updateColors(): Unit =
    for (i <- colors.indices) {
      colors(i) = Color.HSBtoRGB(i / colorCount.toFloat, saturation, brightness)
    }


  updateColors()

  def top = new MainFrame {
    title = "ColorMapSb"
    contents = ui
  }

  /** Util method */
  def print(arr: Array[Int], msg: String = "Array"): Unit = {
    println(s"$msg: size: ${arr.size}")
    arr.foreach(println(_))
    println("-------------------------------")
  }

  /** Util method */
  def p(n: Int): String = {
    f"${Integer.toBinaryString(n)}%32s" replace(' ', '0')
  }
}
