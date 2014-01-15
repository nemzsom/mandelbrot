package hu.n.zs.sandbox

import scala.swing.Swing._
import scala.swing.{MainFrame, Panel, SimpleSwingApplication}
import java.awt.Graphics2D
import java.awt.image.{DataBufferInt, BufferedImage}
import hu.n.zs.mandelbrot.{Area, Point}

object AreaSB extends SimpleSwingApplication {

  val width = 640
  val height = 480

  lazy val ui = new Panel {
    preferredSize = (width, height)

    var bufferedImage = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
    val raster = bufferedImage.getRaster
    val databuffer: DataBufferInt = raster.getDataBuffer.asInstanceOf[DataBufferInt]
    val pixelData = databuffer.getData

    val area = Area(Point(0, 0, 1.0), 0.1, width, height)
    val areaData = area.data

    val colors = new Array[Int](256)

    {
      val rnd = new java.util.Random()
      (0 until 256) foreach { i =>
        colors(i) = 0xFF000000 | i << (rnd.nextInt(3) * 8)
      }
      area update { point =>
        point.iter = rnd.nextInt(256)
      }
    }

    override def paintComponent(g: Graphics2D) = {
      super.paintComponent(g)
      (0 until width * height).foreach { i =>
        pixelData(i) = colors(areaData(i).iter)
      }
      g.drawImage(bufferedImage, 0, 0, bufferedImage.getWidth, bufferedImage.getHeight, null)
    }
  }

  def top = new MainFrame {
    title = "AreaSB"
    contents = ui
  }

}
