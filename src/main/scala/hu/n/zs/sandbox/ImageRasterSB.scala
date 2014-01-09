package hu.n.zs.sandbox

import scala.swing.Swing._
import scala.swing.{Panel, MainFrame, SimpleSwingApplication}
import java.awt.{Graphics2D, Color}
import java.awt.image.{DataBufferInt, SampleModel, Raster, BufferedImage}
import java.util

object ImageRasterSB extends SimpleSwingApplication {

  val width = 640
  val height = 480

  lazy val ui = new Panel {
    background = Color.white
    preferredSize = (width, height)
    val cm = new IterationColorModel(width + height)

    val buffer = {
      val data = new Array[Int](width * height)
      for {
        x <- 0 until width
        y <- 0 until height
        i = x + (y * width)
      } data(i) = x + y
      new DataBufferInt(data, data.size)
    }
    val raster = Raster.createPackedRaster(buffer, width, height, 32, null)
    var bufferedImage: BufferedImage = new BufferedImage(cm, raster, false, null)


    bufferedImage = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB)

    for {
      x <- 0 until width
      y <- 0 until height
      i = x + (y * width)
    } bufferedImage.setRGB(x, y, i << 24)

    override def paintComponent(g: Graphics2D) = {
      super.paintComponent(g)
      g.drawImage(bufferedImage, 0, 0, bufferedImage.getWidth(), bufferedImage.getHeight(), null)
    }
  }



  def top = new MainFrame {
    title = "ImageRasterRB"
    contents = ui
  }

}
