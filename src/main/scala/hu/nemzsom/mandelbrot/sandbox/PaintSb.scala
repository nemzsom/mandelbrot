package hu.nemzsom.sandbox

import scala.swing.Swing._
import scala.swing.{MainFrame, Panel, SimpleSwingApplication}
import java.awt.{Rectangle, Graphics2D}
import java.awt.image.{DataBufferInt, BufferedImage}
import scala.concurrent._
import ExecutionContext.Implicits.global


object PaintSb extends SimpleSwingApplication {

  val width = 640
  val height = 480

  lazy val ui = new Panel {
    preferredSize = (width, height)

    var bufferedImage = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
    val raster = bufferedImage.getRaster
    val databuffer: DataBufferInt = raster.getDataBuffer.asInstanceOf[DataBufferInt]
    val pixelData = databuffer.getData

    override def paintComponent(g: Graphics2D) = {
      super.paintComponent(g)
      g.drawImage(bufferedImage, 0, 0, bufferedImage.getWidth, bufferedImage.getHeight, null)
    }
    
    future {
      Thread.sleep(2000)
      for (i <- pixelData.indices) (pixelData(i) = 255)

      repaint(new Rectangle(200, 300, 100, 100))
      //Thread.sleep(1000)
      repaint(new Rectangle(100, 100, 100, 100))
      //repaint()
    }
  }

  def top = new MainFrame {
    title = "PaintSB"
    contents = ui
  }

}
