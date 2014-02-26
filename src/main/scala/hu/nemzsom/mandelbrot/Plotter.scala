package hu.nemzsom.mandelbrot

import java.awt.image.{DataBufferInt, BufferedImage}
import com.typesafe.scalalogging.slf4j.Logger
import org.slf4j.LoggerFactory
import scala.annotation.tailrec

trait Plotter {

  def plot(p: Point): Unit

  def finish(points: Traversable[Point], cancel: () => Boolean): Unit

}

class BImagePlotter(img: BufferedImage, colorMap: ColorMap) extends Plotter {

  protected val logger: Logger = Logger(LoggerFactory getLogger "mandelbrot.BImagePlotter")

  val pixels = {
    val raster = img.getRaster
    val databuffer: DataBufferInt = raster.getDataBuffer.asInstanceOf[DataBufferInt]
    databuffer.getData
  }

  def plot(p: Point): Unit = pixels(p.index) = colorMap.color(p)

  def finish(points: Traversable[Point], cancel: () => Boolean): Unit = colorMap.finish match {
    case Some(map) =>
      @tailrec def colorAll(ps: Traversable[Point]): Unit = {
        if (!ps.isEmpty && !cancel()) {
          val p = ps.head
          pixels(p.index) = map.color(p)
          colorAll(ps.tail)
        }

      }
      colorAll(points)
    case None =>
  }
}
