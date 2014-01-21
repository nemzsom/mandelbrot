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
    setDebugArea(area)

    var image = new BufferedImage(area.width, area.height, BufferedImage.TYPE_INT_RGB)
    preferredSize = (area.width, area.height)

    focusable = true
    listenTo(this, mouse.moves, mouse.clicks, mouse.wheel)

    reactions += {
      case e: MousePressed => println(s"clicked: ${e.point}")
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

    val raster = image.getRaster
    val databuffer: DataBufferInt = raster.getDataBuffer.asInstanceOf[DataBufferInt]
    val pixelData = databuffer.getData
    val areaData = area.data

    val maxIter = 300

    def update(updater: Updater): Unit = {
      val time = System.nanoTime
      updater.update.onSuccess {
        case updater =>
          (0 until pixelData.size).foreach { i =>
            val point = areaData(i)
            //logPoint(s"coloring to ${color(point)}", point)
            pixelData(i) = color(point)
            /*if ((debugPointAt.x - 5 to debugPointAt.x + 5).contains(point.x) && point.y == debugPointAt.y ||
                point.x == debugPointAt.x && (debugPointAt.y - 5 to debugPointAt.y + 5).contains(point.y)) {
              pixelData(i) = 255 << 16
            }*/
          }
          println(s"render time: ${(System.nanoTime - time) / 1000000} ms")
          repaint()
          if (updater.maxIter < maxIter){
            //Thread.sleep(1500)
            update(updater)
          }
      }
    }
  }

  def top = new MainFrame {
    title = "Mandelbrot set"
    ui.update(new Updater(ui.area, 300))
    contents = ui
  }
}
