package hu.n.zs.mandelbrot

import scala.swing.Swing._
import scala.swing._
import scala.swing.event._
import java.awt.{ Color, Graphics2D }
import java.awt.image.BufferedImage
import java.awt.image.BufferedImageOp
import scala.util.Try
import java.awt.RenderingHints

object MandelbrotApp extends SimpleSwingApplication {

  object State {
    var maxIter = 20
    var area = Area.initialize(Point(0, 0), Point(200, 200), Complex(-2, -2), 2)
    println(area)
  }

  import State._

  lazy val ui = new Panel {

    val blackRgb = Color.BLACK.getRGB
    preferredSize = (area.width, area.height)
    var bufferedImage: BufferedImage = new BufferedImage(area.width, area.height, BufferedImage.TYPE_INT_ARGB)

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
      g.drawImage(bufferedImage, 0, 0, area.width, area.height, null)
    }
    
    def resize: Unit = {
      if (area.width != size.width || area.height != size.height) {
        area = area.resize(size.width - 1, size.height - 1)
        bufferedImage = new BufferedImage(size.width, size.height, BufferedImage.TYPE_INT_ARGB)
        updateImage
        repaint
      }
    }
    
    def updateImage: Unit = {
      var time = System.nanoTime
      area.foreach { case (Point(x, y), complex) =>
        //println(s"x: $x; y: $y; comp: $complex")
        val iter = mandelbrot(complex)
        bufferedImage.setRGB(x, y, getColor(iter))
      }
      time = System.nanoTime - time
      println(s"render time: ${time / 1000000} ms")
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
}