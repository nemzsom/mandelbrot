package hu.nemzsom.mandelbrot

import java.awt.Color
import scala.collection.mutable.{ArrayBuffer, SynchronizedBuffer}
import java.util.concurrent.atomic.AtomicInteger

trait ColorMap {

  def color(point: Point): Int

  def finish: Option[ColorMap] = None
}

object ColorMap {

  def HSBCycleMap(nOfColors: Int): Int => Int = { index =>
    Color.HSBtoRGB(index / nOfColors.toFloat, 0.7f, 0.9f)
  }

  def RGBCycleMap(nOfColors: Int): Int => Int = { index =>
    val k = index.toDouble / (nOfColors - 1)
    def shiftFromDistance(pos: Double): Int = {
      val dist = Math.abs(pos - k)
      if (dist > 1.0 / 8) 0
      else 255 - (dist * 8 * 255).toInt
    }
    if (k <= 1.0 / 8) {
      val s = shiftFromDistance(0)
      s << 16 | s << 8 | s
    } else if ( k >= 7.0 / 8) {
      val s = shiftFromDistance(1)
      s << 16 | s << 8 | s
    }
    else {
      val r = shiftFromDistance(1.0 / 4)
      val g = shiftFromDistance(1.0 / 2)
      val b = shiftFromDistance(3.0 / 4)
      r << 16 | g << 8 | b
    }
  }

  def fromColors(colors: (Int, Float, Float)*)(nOfColors: Int): Int => Int = {
    require(colors.size > 1)
    val transitionRange = 1.0f / colors.size
    println(s"transRange: $transitionRange")
    def get_S_or_B(from: Float, to: Float, pos: Float): Float = {
      val dist = to - from
      from + dist * pos
    }
    def getHue(from: Int, to: Int, pos: Float): Int = {
      val dist = to - from
      if (Math.abs(dist) > 180) {
        if (to > from) getHue(from + 360, to, pos)
        else getHue(from, to + 360, pos)
      }
      else {
        (from + dist * pos).toInt % 360
      }
    }
    def f(index: Int): Int = {
      val k = index.toFloat / (nOfColors)
      val i = (k * colors.size).toInt
      val (fromH, fromS, fromB) = colors(i)
      val (toH, toS, toB) = if (i + 1 == colors.size) colors(0) else colors(i+1)
      val pos = (k % transitionRange) * colors.size
      val h = getHue(fromH, toH, pos)
      val s = get_S_or_B(fromS, toS, pos)
      val b = get_S_or_B(fromB, toB, pos)
      println(s"index: $index, k: $k, i: $i, pos: $pos. ($h, $s, $b)")
      Color.HSBtoRGB(h / 360f, s, b)
    }
    f
  }
}

class Black_and_WhiteColorMap extends ColorMap {

  def color(point: Point): Int = point.location match {
    case Outside(iter) => if (iter % 2 == 0) 0 else 0xFFFFFFFF
    case _ => 0 // Inside or Unsettled
  }
}

class LinearColorMap(nOfColors: Int, mapFunc: Int => (Int => Int)) extends ColorMap {

  val colorMap: Array[Int] = {
    val f = mapFunc(nOfColors)
    var i = -1
    Array.fill(nOfColors) {
      i += 1
      f(i)
    }
  }

  def color(point: Point): Int = point.location match {
    case Outside(iter) => colorMap(iter % nOfColors)
    case _ => 0 // Inside or Unsettled
  }
}

class HistogramColorMap(nOfColors: Int, mapFunc: Int => (Int => Int)) extends LinearColorMap(nOfColors, mapFunc) {

  val histogram = new ArrayBuffer[Int] {

    val fillArr = new Array[Int](100)

    private def fillTo(i: Int): Unit = while (i >= size) this ++= fillArr

    override def apply(i: Int): Int = {
      fillTo(i)
      super.apply(i)
    }

    override def update(i: Int, elem: Int): Unit = {
      fillTo(i)
      super.update(i, elem)
    }
  }

  override def color(point: Point): Int = point.location match {
    case Outside(iter) =>
      synchronized { histogram(iter) += 1 }
      colorMap(iter % nOfColors)
    case _ => 0 // Inside or Unsettled
  }

  override def finish: Option[ColorMap] = {
    var actCount = 0
    val histArr = histogram.toArray.map { count =>
      actCount += count
      actCount
    }
    val total = histogram.sum
    println(s"total: $total, size: ${histogram.size}")
    println(histogram.mkString(","))
    println(histArr.mkString(","))
    Option(new ColorMap {
      override def color(point: Point): Int = point.location match {
        case Outside(iter) =>
          val count = histArr(iter)
          colorMap(count * (nOfColors-1) / total)
        case _ =>  0 // Inside or Unsettled
      }
    })
  }
}