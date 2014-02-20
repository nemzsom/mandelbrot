package hu.nemzsom.mandelbrot

import java.awt.image.{DataBufferInt, BufferedImage}

trait Plotter {

  def plot(p: Point): Unit

  def finish(points: Traversable[Point]): Unit

}

class BImagePlotter(img: BufferedImage, colorMap: ColorMap) extends Plotter {

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
      points foreach { p =>
        pixels(p.index) = map.color(p)
      }
    case None =>
  }
}
