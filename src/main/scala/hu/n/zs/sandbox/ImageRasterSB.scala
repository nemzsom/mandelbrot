package hu.n.zs.sandbox

import scala.swing.Swing._
import scala.swing.{Panel, MainFrame, SimpleSwingApplication}
import java.awt.{Graphics2D, Color}
import java.awt.image._
import java.util
import sun.security.provider.certpath.IndexedCollectionCertStore

object ImageRasterSB extends SimpleSwingApplication {

  val width = 640
  val height = 480

  lazy val ui = new Panel {
    background = Color.white
    preferredSize = (width, height)

    // test custom implementation
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

    // test rgb
    bufferedImage = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
    val max = width + height
    for {
      x <- 0 until width
      y <- 0 until height
      p = 255 * (x + y) / max
    } bufferedImage.setRGB(x, y, p << 8)

    // test IndexColorModel
    /*val colorMapSize = width + height
    val r, g, b = new Array[Byte](colorMapSize)
    for {
      i <- 0 until colorMapSize
      c: Byte = (255 * i / colorMapSize).toByte
    } { r(i) = c}
    val icm = new IndexColorModel(8, colorMapSize, r, g, b)
    bufferedImage = new BufferedImage(width, height, BufferedImage.TYPE_BYTE_INDEXED)
    for {
      x <- 0 until width
      y <- 0 until height
    } bufferedImage.getRaster.setSample(x, y, 0, x + y)*/


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
