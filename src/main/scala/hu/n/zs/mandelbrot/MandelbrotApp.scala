package hu.n.zs.mandelbrot

import scala.swing.Swing._
import scala.swing.{MainFrame, Panel, SimpleSwingApplication}
import java.awt.{Color, Graphics2D}
import scala.swing.event._
import scala.swing.event.MousePressed
import scala.swing.event.MouseWheelMoved
import scala.swing.event.MouseDragged
import scala.swing.event.UIElementResized
import java.awt.image.{DataBufferInt, BufferedImage}

object MandelbrotApp extends SimpleSwingApplication {

  lazy val ui = new Panel {

    var area = Area(Complex(-2, -2), 4, 640, 480)
    var image = new BufferedImage(area.width, area.height, BufferedImage.TYPE_INT_RGB)
    preferredSize = (area.width, area.height)

    focusable = true
    listenTo(this, mouse.moves, mouse.clicks, mouse.wheel)

    reactions += {
      case e: MousePressed =>
      case e: MouseDragged =>
      case e: MouseWheelMoved =>
      case _: UIElementResized =>
      case _: FocusLost => repaint()
    }

    override def paintComponent(g: Graphics2D) = {
      super.paintComponent(g)
      g.drawImage(image, 0, 0, image.getWidth, image.getHeight, null)
    }



    // test code

    val maxIter = 5000
    val colorMap = new Array[Int](maxIter + 1)
    for (i <- 0 until maxIter) {
      colorMap(i) =  {
        val c=3*math.log(i)/math.log(maxIter-1.0)
        if(c<1) (255*c).toInt << 16 //new Color((255*c).toInt, 0, 0).getRGB
        else if(c<2) 0x00FF0000 | (255*c).toInt << 8 //new Color(255, (255*(c-1)).toInt, 0).getRGB
        else 0x00FFFF00 | (255*c).toInt// new Color(255, 255, (255*(c-2)).toInt).getRGB
      }
    }
    colorMap(maxIter) == 0x00FFFFFF
    def update(): Unit = {
      val time = System.nanoTime
      val mandelbrot = Mandelbrot_deprecated(maxIter)
      area.update { point =>
        point.iter = mandelbrot(point.iterValue)
      }
      val raster = image.getRaster
      val databuffer: DataBufferInt = raster.getDataBuffer.asInstanceOf[DataBufferInt]
      val pixelData = databuffer.getData
      val areaData = area.data
      (0 until pixelData.size).foreach { i =>
        val iter = areaData(i).iter
        pixelData(i) = if (iter < 0) Color.RED.getRGB else colorMap(iter)
      }
      println(s"render time: ${(System.nanoTime - time) / 1000000} ms")
      repaint()
    }

    update()
  }

  def top = new MainFrame {
    title = "Mandelbrot set"
    contents = ui
  }

}
