package hu.nemzsom.mandelbrot

import scala.swing.Swing._
import scala.swing._
import scala.swing.event._
import java.awt.{ Color, Graphics2D }
import java.awt.image.BufferedImage

@deprecated
object MandelbrotGui_deprecated extends SimpleSwingApplication {

  object State {
    var maxIter = 300
    var area = Area_deprecated.initialize(Point_deprecated(0, 0), 1000, 1000, Complex(-2, -2), 2)
  }

  import State._

  lazy val ui = new Panel {

    val blackRgb = Color.BLACK.getRGB
    preferredSize = (area.width, area.height)
    var bufferedImage: BufferedImage = new BufferedImage(area.width, area.height, BufferedImage.TYPE_INT_RGB)
    var draggedFrom: Point_deprecated = (0, 0)

    val mandelbrot = Mandelbrot_deprecated(maxIter)
    focusable = true
    listenTo(this, mouse.moves, mouse.clicks, mouse.wheel)

    val colorMap = new Array[Int](maxIter + 1)
    for (i <- 0 until maxIter) {
      colorMap(i) =  {
        val c=3*math.log(i)/math.log(maxIter-1.0)
        if(c<1) (255*c).toInt << 16
        else if(c<2) 0x00FF0000 | (255*c).toInt << 8
        else 0x00FFFF00 | (255*c).toInt
      }
    }
    colorMap(maxIter) = 0

    updateImage()

    reactions += {
      case e: MousePressed => draggedFrom = e.point
      case e: MouseDragged =>
        val diffX = e.point.x - draggedFrom.x
        val diffY = e.point.y - draggedFrom.y
        draggedFrom = e.point
        area = area.move(diffX, diffY)
        updateImage()
        repaint()
      case e: MouseWheelMoved =>
        area = area zoom (1 - e.rotation * 0.25, e.point)
        updateImage()
        repaint()
      case _: UIElementResized => resize()
      case _: FocusLost => repaint()
    }

    override def paintComponent(g: Graphics2D) = {
      super.paintComponent(g)
      g.drawImage(bufferedImage, 0, 0, area.width, area.height, null)
    }
    
    def resize(): Unit = {
      if (area.width != size.width || area.height != size.height) {
        area = area.resize(size.width, size.height)
        bufferedImage = new BufferedImage(size.width, size.height, BufferedImage.TYPE_INT_RGB)
        updateImage()
        repaint()
      }
    }
    
    def updateImage(): Unit = {
      var time = System.nanoTime
      area.foreach { case (Point_deprecated(x, y), complex) =>
        val iter = mandelbrot(complex)
        bufferedImage.setRGB(x, y, /*if (iter % 2 == 0) 0 else 0xFFFFFFFF*/ colorMap(iter))
      }
      time = System.nanoTime - time
      println(s"render time: ${time / 1000000} ms")
    }
  }

  def top = new MainFrame {
    title = "Mandelbrot set"
    contents = ui
  }
}