package hu.nemzsom.mandelbrot

import java.awt.image.{DataBufferInt, BufferedImage}
import com.typesafe.scalalogging.slf4j.Logger
import org.slf4j.LoggerFactory

trait Plotter {

  def plot(p: Point): Unit

  def finish(points: Traversable[Point]): Unit

}

class BImagePlotter(img: BufferedImage, colorMap: ColorMap) extends Plotter {

  protected val logger: Logger = Logger(LoggerFactory getLogger "mandelbrot.BImagePlotter")

  val pixels = {
    val raster = img.getRaster
    val databuffer: DataBufferInt = raster.getDataBuffer.asInstanceOf[DataBufferInt]
    databuffer.getData
  }

  // DEBUG
  debugPixels = pixels
  // DEBUG END

  def plot(p: Point): Unit = pixels(p.index) = colorMap.color(p)

  def finish(points: Traversable[Point]): Unit = colorMap.finish match {
    case Some(map) =>
      // DEBUG
      val time = System.nanoTime
      // DEBUG END
      points foreach { p =>
        pixels(p.index) = map.color(p)
      }
      logger.info(s"AFTER_COLOR done in ${(System.nanoTime - time) / 1000000} ms")
    case None =>
  }
}
