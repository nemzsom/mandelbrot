package hu.nemzsom.mandelbrot

import java.awt.Color
import scala.collection.mutable.{ArrayBuffer, SynchronizedBuffer}

trait ColorMap {

  def color(point: Point): Int
}

class Black_and_WhiteColorMap extends ColorMap {

  def color(point: Point): Int = point.location match {
    case Outside(iter) => if (iter % 2 == 0) 0 else 0xFFFFFFFF
    case _ => 0 // Inside or Unsettled
  }
}

class LinearColorMap(nOfColors: Int) extends ColorMap {

  val saturation = 0.7f
  val brightness = 0.9f

  val colorMap: Array[Int] = {
    var i = -1
    Array.fill(nOfColors) {
      i += 1
      Color.HSBtoRGB(i / nOfColors.toFloat, saturation, brightness)
    }
  }

  def color(point: Point): Int = point.location match {
    case Outside(iter) => colorMap(iter % nOfColors)
    case _ => 0 // Inside or Unsettled
  }
}

class HistogramColorMap(nOfColors: Int) extends LinearColorMap(nOfColors) {

  val histogram = new ArrayBuffer[Int] with SynchronizedBuffer[Int]

  override def color(point: Point): Int = point.location match {
    case Outside(iter) =>
      histogram(iter) += 1
      colorMap(iter % nOfColors)
    case _ => 0 // Inside or Unsettled
  }

  def finalize
}