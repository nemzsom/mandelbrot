package hu.n.zs.sandbox

import scala.swing.Swing._
import scala.swing.{Component, MainFrame, Panel, SimpleSwingApplication}
import java.awt.{Color, Graphics2D}
import java.awt.image.{DataBufferInt, BufferedImage}
import hu.n.zs.mandelbrot.{Area, Point}
import scala.concurrent._
//import ExecutionContext.Implicits.global
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.Executors

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
    (0 until 256) foreach { i =>
      colors(i) = 0xFF000000 | i << 8
    }

    // red lines
    area.subArea(0, 0, area.width - 1, 1).update(_.iter = -1)
    area.subArea(area.width - 1, 0, 1, area.height - 1).update(_.iter = -1)
    area.subArea(1, area.height - 1, area.width - 1, 1).update(_.iter = -1)
    area.subArea(0, 1, 1, area.height - 1).update(_.iter = -1)

    val animator = new Animator(area, this)

    override def paintComponent(g: Graphics2D) = {
      super.paintComponent(g)
      (0 until width * height).foreach { i =>
        val iter = areaData(i).iter
        pixelData(i) = if (iter < 0) Color.RED.getRGB else colors(iter)
      }
      g.drawImage(bufferedImage, 0, 0, bufferedImage.getWidth, bufferedImage.getHeight, null)
    }
  }

  def top = new MainFrame {
    title = "AreaSB"
    contents = ui
  }

  class Animator(area: Area, comp: Component) {

    val width = area.width
    val height = area.height

    val workAreas = init(area.subArea(1, 1, area.width - 2, area.height -2), List.empty[Area])

    animate(0)

    def animate(step: Int): Unit = {
      val counter = new AtomicInteger(workAreas.size - 1)
      def runOnstart = {
        val c = counter.getAndDecrement
        println(s"started $c on ${Thread.currentThread()}")
        Thread.sleep(1000)
        if (c == 0){
          comp.repaint()
          animate(step + 1)
        }
      }
      workAreas.foreach(updateArea(step, _, runOnstart))
    }

    def init(area: Area, workAreas: List[Area]): List[Area] = {
      if (area.height < 20 || area.width < 20) area :: workAreas
      else {
        if (area.height > area.width) {
          val halfY = area.height / 2
          val extendedAreas = init(area.subArea(0, 0, area.width, halfY - 1), workAreas)
          area.subArea(0, halfY, area.width, 1).update(_.iter = -1)
          init(area.subArea(0, halfY + 1, area.width, halfY - 1), extendedAreas)
        }
        else {
          val halfX = area.width / 2
          val extendedAreas = init(area.subArea(0, 0, halfX -1 , area.height), workAreas)
          area.subArea(halfX, 0, 1, area.height).update(_.iter = -1)
          init(area.subArea(halfX + 1, 0, halfX -1 , area.height), extendedAreas)
        }
      }
    }

    def updateArea(step: Int, area: Area, runOnStart: => Unit): Unit = {
      //implicit val ec: ExecutionContext = ExecutionContext.fromExecutor(Executors.newSingleThreadExecutor)
      implicit val ec = new ExecutionContext {
        val threadPool = Executors.newSingleThreadExecutor

        def execute(runnable: Runnable) {
          threadPool.submit(runnable)
        }

        def reportFailure(t: Throwable) {}
      }
      future {
        runOnStart
        area update { point =>
          point.iter = (255 * (point.x + point.y) / (width + height) + step) % 255
        }
      }
    }
  }
}
