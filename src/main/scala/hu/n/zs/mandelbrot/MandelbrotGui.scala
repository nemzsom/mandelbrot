package hu.n.zs.mandelbrot

import scala.swing.Swing._
import scala.swing._
import scala.swing.event._
import java.awt.{ Color, Graphics2D, Point }
import java.awt.image.BufferedImage
import java.awt.image.BufferedImageOp
import scala.util.Try

object MandelbrotApp extends SimpleSwingApplication {

  object Config {
    // picture pane
    val xMax: Int = 1024
    val yMax: Int = 768
    // complex pane
    val reMin: Double = -2.5
    val reMax: Double = 1
    val imMin: Double = -1
    val imMax: Double = 1
  }

  import Config._

  lazy val ui = new Panel {

    val blackRgb = Color.BLACK.getRGB
    preferredSize = (xMax, yMax)
    val bufferedImage: BufferedImage = new BufferedImage(xMax, yMax, BufferedImage.TYPE_INT_ARGB)

    val mandelbrot = Mandelbrot(500)
    focusable = true
    listenTo(this)

    for {
      x <- 0 until xMax
      val re = scaleRe(x)
      y <- 0 until yMax
      val im = scaleIm(y)
    } {
      if (mandelbrot(new Complex(re, im))) {
        bufferedImage.setRGB(x, y, blackRgb)
      }
    }

    reactions += {
      case _: FocusLost => repaint()
    }

    override def paintComponent(g: Graphics2D) = {
      super.paintComponent(g)
      g.drawImage(bufferedImage, 0, 0, xMax, yMax, null)
    }
  }

  def top = new MainFrame {
    title = "Mandelbrot set"
    contents = ui
  }

  def scale(n: Int)(maxN: Int, from: Double, to: Double): Double = n * (to - from) / maxN + from
    //(n / (maxN / (to - from))) + from

  def scaleRe(n: Int) = scale(n)(xMax, reMin, reMax)
  def scaleIm(n: Int) = scale(n)(yMax, imMin, imMax)

}