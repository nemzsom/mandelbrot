package hu.nemzsom.mandelbrot

import java.awt.Color
import scala.collection.mutable.ArrayBuffer

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

abstract class ContinuousColorMap(colorMarks: List[ColorMark]) extends ColorMap {

  require(colorMarks.size > 1, "need least 2 colors")
  require(colorMarks.head.position == 0.0f, "the first colorMark should begin at position 0.0")
  require(
    colorMarks.sliding(2).forall { case List(cm1, cm2) =>
      cm1.position <= cm2.position
    }, "colorMarks must be in ascending order"
  )

  private val colorRanges: List[(ColorMark, ColorMark)] = {
    val contColorMarks =
      if (colorMarks.last.position == 1.0) colorMarks
      else {
        val firstCM = colorMarks.head
        val lastCM = ColorMark(firstCM.hue, firstCM.saturation, firstCM.brightness, 1f)
        colorMarks :+ lastCM
      }
    contColorMarks.sliding(2).map{ case List(cm1, cm2) => (cm1, cm2)}.toList
  }
      
  def map(k: Float): Int = {
    val (cm1, cm2) = colorRanges.find { case (_cm1, _cm2) => k >= _cm1.position && k <= _cm2.position }.get
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
      map(i / nOfColors)
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
}

class Grayscale_ColorMap(val nOfColors: Int) extends ContinuousColorMap(List(ColorMark(0, 0, 0, 0), ColorMark(0, 0, 1, 0.5f))) with IndexedColorMap
class Black_And_White_ColorMap extends Grayscale_ColorMap(2)
class Blue_Yellow_ColorMap(val nOfColors: Int) extends ContinuousColorMap(List(ColorMark(236, 0.9f, 0.25f, 0),
                                                                               ColorMark(236, 0.9f, 0.9f, 1f/5),
                                                                               ColorMark(236, 0, 0.9f, 1f/5*2),
                                                                               ColorMark(44, 0, 0.9f, 1f/5*2),
                                                                               ColorMark(44, 0.9f, 0.9f, 1f/5*3),
                                                                               ColorMark(44, 0.9f, 0.25f, 1f/5*4))) with IndexedColorMap

trait HistogramColorMap extends IndexedColorMap {

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
    val orig = this
    Option(new ColorMap {
      override def color(point: Point): Int = point.location match {
        case Outside(iter) =>
          val count = histArr(iter)
          colorMap(count * (nOfColors-1) / total)
        case _ =>  0 // Inside or Unsettled
      }

      override def map(k: Float): Int = orig.map(k)
    })
  }
}

trait SmoothColorMap extends ColorMap {

  val nOfColors: Int

  override def finish: Option[ColorMap] = {
    val orig = this
    Option(new ColorMap {
      override def color(point: Point): Int = point.location match {
        case Outside(iter) =>
          if (iter > point.iter) Calculator.iterate(point, iter)
          val smoothIter = iter + 1 - Math.log(Math.log(!point.iterValue)) / Math.log(2)
          map((smoothIter % nOfColors).toFloat / nOfColors)
        case _ => 0 // Inside or Unsettled
      }

      override def map(k: Float): Int = orig.map(k)
    })
  }
}