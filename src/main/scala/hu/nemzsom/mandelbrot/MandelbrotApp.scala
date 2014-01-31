package hu.nemzsom.mandelbrot

import scala.swing.Swing._
import scala.swing.{MainFrame, Panel, SimpleSwingApplication}
import java.awt.Graphics2D
import scala.swing.event._
import scala.swing.event.MousePressed
import scala.swing.event.MouseWheelMoved
import scala.swing.event.MouseDragged
import scala.swing.event.UIElementResized
import java.awt.image.{DataBufferInt, BufferedImage}
import java.util.concurrent.{Executors, ThreadPoolExecutor}
import scala.concurrent.ExecutionContext

object MandelbrotApp extends SimpleSwingApplication {

  val numOfProcs = Runtime.getRuntime.availableProcessors
  val executor: ThreadPoolExecutor = Executors.newFixedThreadPool(numOfProcs * 2).asInstanceOf[ThreadPoolExecutor]
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutor(executor)

  lazy val ui = new Panel with Calculator with Renderer {

    val width = 200
    val height = 200
    val mainArea = Area(Complex(-2, -2), 4, width, height)
    setDebugArea(mainArea)

    var image = new BufferedImage(mainArea.width, mainArea.height, BufferedImage.TYPE_INT_RGB)
    preferredSize = (mainArea.width, mainArea.height)
    val raster = image.getRaster
    val databuffer: DataBufferInt = raster.getDataBuffer.asInstanceOf[DataBufferInt]
    val pixels = databuffer.getData
    val painter = this

    for (i <- pixels.indices) (pixels(i) = 0x00bebebe)

    focusable = true
    listenTo(this, mouse.moves, mouse.clicks, mouse.wheel)

    reactions += {
      case e: MousePressed => /*println(s"clicked: ${e.point}");*/ debugQuene.put(0)
      case e: MouseDragged =>
      case e: MouseWheelMoved => (1 to 10).foreach(debugQuene.put(_))
      case _: UIElementResized =>
      case _: FocusLost => repaint()
    }

    override def paintComponent(g: Graphics2D) = {
      super.paintComponent(g)
      g.drawImage(image, 0, 0, image.getWidth, image.getHeight, null)
    }
  }

  def top = new MainFrame {
    title = "Mandelbrot set"
    ui.calculate()
    contents = ui
  }
}
