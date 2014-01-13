package hu.n.zs.sandbox

import scala.swing.Swing._
import scala.swing.{Panel, MainFrame, SimpleSwingApplication}
import java.awt.{Graphics2D, Color}
import java.awt.image._
import java.util

object ImageRasterSB extends SimpleSwingApplication {

  val width = 640
  val height = 480

  lazy val ui = new Panel {
    preferredSize = (width, height)

    var bufferedImage = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
    val max = width + height

    val raster = bufferedImage.getRaster

    val databuffer: DataBufferInt = raster.getDataBuffer.asInstanceOf[DataBufferInt]

    val pixelData = databuffer.getData

    for {
      x <- 0 until width
      y <- 0 until height
      p = 255 * (x + y) / max
    } pixelData(y * width + x) = p << 8

    /*
    // rectangle
    for {
      x <- 100 until 200
      y <- 100 until 200
    } pixelData(y * width + x) = Color.BLUE.getRGB*/

    override def paintComponent(g: Graphics2D) = {
      super.paintComponent(g)
      g.drawImage(bufferedImage, 0, 0, bufferedImage.getWidth, bufferedImage.getHeight, null)
    }
  }



  def top = new MainFrame {
    title = "ImageRasterRB"
    contents = ui
  }

}
