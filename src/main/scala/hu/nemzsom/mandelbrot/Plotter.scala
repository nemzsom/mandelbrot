package hu.nemzsom.mandelbrot

import java.awt.image.{DataBufferInt, BufferedImage}

trait Plotter {

  def plot(p: Point): Unit

}

class BImagePlotter(img: BufferedImage, colorMap: ColorMap) extends Plotter {

  val pixels = {
    val raster = img.getRaster
    val databuffer: DataBufferInt = raster.getDataBuffer.asInstanceOf[DataBufferInt]
    databuffer.getData
  }

  def plot(p: Point): Unit = pixels(p.index) = colorMap.color(p)
}
