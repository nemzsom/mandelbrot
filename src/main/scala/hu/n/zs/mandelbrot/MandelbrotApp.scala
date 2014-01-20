package hu.n.zs.mandelbrot

import scala.swing.Swing._
import scala.swing.{MainFrame, Panel, SimpleSwingApplication}
import java.awt.Graphics2D
import scala.swing.event._
import scala.swing.event.MousePressed
import scala.swing.event.MouseWheelMoved
import scala.swing.event.MouseDragged
import scala.swing.event.UIElementResized
import java.awt.image.{DataBufferInt, BufferedImage}

object MandelbrotApp extends SimpleSwingApplication {

  lazy val ui = new Panel with Calculator with ColorMap {

    val width = 640
    val height = 480
    val area = Area(Complex(-2, -2), 4, width, height)

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

    val maxIter = 3000

    def update(): Unit = {
      val time = System.nanoTime
      calculate(maxIter)
      val raster = image.getRaster
      val databuffer: DataBufferInt = raster.getDataBuffer.asInstanceOf[DataBufferInt]
      val pixelData = databuffer.getData
      val areaData = area.data
      (0 until pixelData.size).foreach { i =>
        val point = areaData(i)
        pixelData(i) = color(point.iter)
      }
      println(s"render time: ${(System.nanoTime - time) / 1000000} ms")
      repaint()
    }
  }

  def top = new MainFrame {
    title = "Mandelbrot set"
    ui.update()
    contents = ui
  }

}
