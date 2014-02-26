package hu.nemzsom.mandelbrot.sandbox

import scala.swing.Swing._
import scala.swing.{MainFrame, Panel, SimpleSwingApplication}
import java.awt.image.BufferedImage
import java.awt.Graphics2D
import hu.nemzsom.mandelbrot._
import hu.nemzsom.mandelbrot.Outside

object ColorMapSb extends SimpleSwingApplication {

  val width = 900
  val height = 50
  val colorCount = 20
  val colorMap = new ColorMap.Brown(colorCount)

  lazy val ui = new Panel {

    preferredSize = (width, height * 2 + 20)

    override def paintComponent(g: Graphics2D) = {
      super.paintComponent(g)
      val sampleWidth = width / 2
      drawImage { x =>
        val p = new Point(Complex.ZERO, 0)
        p.location = Outside(x * colorCount / sampleWidth)
        colorMap.color(p)
      }(0, g)
      drawImage { x =>
        colorMap.map((x.toFloat / sampleWidth) % 1)
      }(height + 20, g)
    }
  }

  def drawImage(x2Color: Int => Int)(fromY: Int, g: Graphics2D) : Unit= {
    val img = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
    for {
      x <- 0 until width
      y <- 0 until height
    } {
      img.setRGB(x, y, x2Color(x))
    }
    g.drawImage(img, 0, fromY, width, height, null)
  }

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
