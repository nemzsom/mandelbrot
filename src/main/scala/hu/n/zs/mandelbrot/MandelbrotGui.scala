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
    // iteration
    val maxIter = 500
  }

  import Config._

  lazy val ui = new Panel {

    val blackRgb = Color.BLACK.getRGB
    preferredSize = (xMax, yMax)
    val bufferedImage: BufferedImage = new BufferedImage(xMax, yMax, BufferedImage.TYPE_INT_ARGB)

    val mandelbrot = Mandelbrot(maxIter)
    focusable = true
    listenTo(this)

    for {
      x <- 0 until xMax
      val re = scaleRe(x)
      y <- 0 until yMax
      val im = scaleIm(y)
    } {
      val iter = mandelbrot(new Complex(re, im))
      bufferedImage.setRGB(x, y, getColor(iter))
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
  
  def getColor(i:Int):Int={
      if (i == 0) return Color.BLACK.getRGB
      val iter = maxIter - i 
      val c=3*math.log(iter)/math.log(maxIter-1.0)
      if(c<1) new Color((255*c).toInt, 0, 0).getRGB
      else if(c<2) new Color(255, (255*(c-1)).toInt, 0).getRGB
      else new Color(255, 255, (255*(c-2)).toInt).getRGB
   }

  def scale(n: Int)(maxN: Int, from: Double, to: Double): Double = n * (to - from) / maxN + from
    //(n / (maxN / (to - from))) + from

  def scaleRe(n: Int) = scale(n)(xMax, reMin, reMax)
  def scaleIm(n: Int) = scale(n)(yMax, imMin, imMax)

}