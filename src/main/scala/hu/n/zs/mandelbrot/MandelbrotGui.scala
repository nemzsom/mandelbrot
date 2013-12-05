package hu.n.zs.mandelbrot

import scala.swing.Swing._
import scala.swing._
import scala.swing.event._
import java.awt.{ Color, Graphics2D, Point }
import java.awt.image.BufferedImage
import java.awt.image.BufferedImageOp
import scala.util.Try
import java.awt.RenderingHints

object MandelbrotApp extends SimpleSwingApplication {

  object State {
    // picture pane
    var xMax: Int = 200
    var yMax: Int = 20
    // complex pane
    var reMin: Double = -2
    var reMax: Double = 2
    var imMin: Double = -2
    var imMax: Double = 10
    // iteration
    var maxIter = 10
    def refresh: Unit = {
      imMax = xMax.toDouble/yMax*(reMax - reMin) + imMin
      println(s"refresh. imMax: $imMax; xMax: $xMax, yMax: $yMax, reMax: $reMax, reMin: $reMin, imMin: $imMin")
    }
    refresh
  }

  import State._

  lazy val ui = new Panel {

    val blackRgb = Color.BLACK.getRGB
    preferredSize = (xMax, yMax)
    var bufferedImage: BufferedImage = new BufferedImage(xMax, yMax, BufferedImage.TYPE_INT_ARGB)

    val mandelbrot = Mandelbrot(maxIter)
    focusable = true
    listenTo(this)

    updateImage

    reactions += {
      case _: UIElementResized => resize
      case _: FocusLost => repaint()
    }

    override def paintComponent(g: Graphics2D) = {
      super.paintComponent(g)
      g.drawImage(bufferedImage, 0, 0, xMax, yMax, null)
    }
    
    def resize: Unit = {
      if (xMax != size.width || yMax != size.height) {
        val (drawX, drawY) = calculateAspectRatioFit(xMax, yMax, size.width, size.height)
        val newImg = new BufferedImage(size.width, size.height, BufferedImage.TYPE_INT_ARGB)
        val graphics = newImg.createGraphics
        graphics.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BILINEAR)
        graphics.setRenderingHint(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY)
        graphics.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
        graphics.drawImage(bufferedImage, 0, 0, drawX, drawY, null)
        bufferedImage = newImg
        repaint
        xMax = size.width
        yMax = size.height
        State.refresh
        updateImage
        repaint
      }
    }
    
    def calculateAspectRatioFit(srcWidth: Double, srcHeight: Double, maxWidth: Double, maxHeight: Double): (Int, Int) = {
        val ratio = math.min(maxWidth / srcWidth, maxHeight / srcHeight);
        ((srcWidth*ratio).toInt, (srcHeight*ratio).toInt)
    }
    
    def updateImage: Unit = {
      for {
        x <- 0 until xMax
        val im = scaleIm(x)
        y <- 0 until yMax
        val re = scaleRe(y)
      } {
        val iter = mandelbrot(new Complex(re, im))
        bufferedImage.setRGB(x, y, getColor(iter))
      }
    }
  }

  def top = new MainFrame {
    title = "Mandelbrot set"
    contents = ui
  }
  
  def getColor(iter: Int):Int={
      if (iter == maxIter) return Color.BLACK.getRGB
      val c=3*math.log(iter)/math.log(maxIter-1.0)
      if(c<1) new Color((255*c).toInt, 0, 0).getRGB
      else if(c<2) new Color(255, (255*(c-1)).toInt, 0).getRGB
      else new Color(255, 255, (255*(c-2)).toInt).getRGB
   }

  def scale(n: Int)(maxN: Int, from: Double, to: Double): Double = n * (to - from) / maxN + from

  def scaleRe(n: Int) = scale(n)(yMax, reMin, reMax)
  def scaleIm(n: Int) = scale(n)(xMax, imMin, imMax)

}