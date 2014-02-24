package hu.nemzsom.mandelbrot

import java.awt.Color
import scala.collection.mutable.{ArrayBuffer, SynchronizedBuffer}
import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable

trait ColorMap {
  
  def map(k: Float): Int

  def color(point: Point): Int

  def finish: Option[ColorMap] = None
}

case class ColorMark(hue: Int, saturation: Float, brightness: Float, position: Float) {
  require (
     hue >=0 && hue <=360 &&
     saturation >= 0f && saturation <= 1f &&
     brightness >= 0f && brightness <= 1f &&
     position >= 0f && position <= 1f,
     "hue must between 0 - 360, saturation, brightness and position between 0.0 - 1.0"
  )
}

trait ContinuousColorMap extends ColorMap {

  val colorMarks: List[ColorMark]

  private lazy val colorRanges: List[(ColorMark, ColorMark)] = {
    require(colorMarks.size > 1, "need least 2 colors")
    require(colorMarks.head.position == 0.0f, "the first colorMark should begin at position 0.0")
    require(
      colorMarks.sliding(2).forall { case List(cm1, cm2) =>
        cm1.position < cm2.position
      }, ("colorMarks must be in ascending order")
    )
    val contColorMarks =
      if (colorMarks.last.position == 1.0) colorMarks
      else {
        val firstCM = colorMarks.head
        val lastCM = ColorMark(firstCM.hue, firstCM.saturation, firstCM.brightness, 1f)
        colorMarks :+ lastCM
      }
    contColorMarks.sliding(2) map { case List(cm1, cm2) => (cm1, cm2)} toList
  }
      
  def map(k: Float): Int = {
    val (cm1, cm2) = colorRanges.find { case (cm1, cm2) => k >= cm1.position && k <= cm2.position }.get
    val diff = k - cm1.position
    val cInterval = cm2.position - cm1.position
    def interpolate(from: Float, to: Float): Float = from + (to - from) * diff / cInterval
    val hue =
      if (Math.abs(cm1.hue - cm2.hue) > 180) {
        if (cm2.hue > cm1.hue) interpolate(cm1.hue + 360, cm2.hue)
        else interpolate(cm1.hue, cm2.hue + 360)
      }
      else interpolate(cm1.hue, cm2.hue)
    val sat = interpolate(cm1.saturation, cm2.saturation)
    val bri = interpolate(cm1.brightness, cm2.brightness)
    Color.HSBtoRGB(hue / 360f, sat, bri)
  }
}

trait IndexedColorMap extends ColorMap {

  val nOfColors: Int

  lazy val colorMap: Array[Int] = {
    var i = -1.0f
    Array.fill(nOfColors) {
      i += 1.0f
      map(i / (nOfColors - 1))
    }
  }

  def color(point: Point): Int = point.location match {
    case Outside(iter) => colorMap(iter % nOfColors)
    case _ => 0 // Inside or Unsettled
  }

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
      Color.HSBtoRGB(h / 360f, s, b)
    }
    f
  }

  def Blue_Yellow_Map(nOfColors: Int) = fromColors((236, 1f, 0.36f), (202, 0.44f, 0.95f), (160, 0.06f, 1f), (70, 0.12f, 0.98f), (40, 0.98f, 1f), (346, 0.85f, 0.36f))(nOfColors)
}

class Black_and_WhiteColorMap extends IndexedColorMap with ContinuousColorMap {

  val colorMarks: List[ColorMark] = List(ColorMark(0, 0, 0, 0), ColorMark(0, 0, 1, 1))
  val nOfColors: Int = 2
}

/*class LinearColorMap(nOfColors: Int, mapFunc: Int => (Int => Int)) extends ColorMap {

  /*val colorMap: Array[Int] = {
    val f = mapFunc(nOfColors)
    var i = -1
    Array.fill(nOfColors) {
      i += 1
      f(i)
    }
  }*/

  /*def color(point: Point): Int = point.location match {
    case Outside(iter) => colorMap(iter % nOfColors)
    case _ => 0 // Inside or Unsettled
  }*/
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

class SmoothColorMap(nOfColors: Int, mapFunc: Int => (Int => Int)) extends LinearColorMap(nOfColors, mapFunc) {

  override def finish: Option[ColorMap] = {
    Option(new ColorMap {
      override def color(point: Point): Int = point.location match {
        case Outside(iter) =>
          if (iter > point.iter) Calculator.iterate(point, iter)
          val smoothIter = iter + 1 - Math.log(Math.log(!point.iterValue))/Math.log(2)
          val color1 = colorMap(smoothIter.toInt % nOfColors)
          val color2 = colorMap((smoothIter.toInt + 1) % nOfColors)
          linearInterpolate(color1, color2, smoothIter % 1)
        case _ =>  0 // Inside or Unsettled
      }
    })
  }

  def linearInterpolate(c1: Int, c2: Int, pos: Double): Int = {
    def getRGB(rgb: Int): (Int, Int, Int) =
      ((0x00FF0000 & rgb) >> 16, (0x0000FF00 & rgb) >> 8, 0x000000FF & rgb)
    def interpolate(a: Int, b: Int): Int = (a + (b - a) * pos).toInt
    val (r1, g1, b1) = getRGB(c1)
    val (r2, g2, b2) = getRGB(c2)
    val r = interpolate(r1, r2)
    val g = interpolate(g1, g2)
    val b = interpolate(b1, b2)
    r << 16 | g << 8 | b
  }
}*/