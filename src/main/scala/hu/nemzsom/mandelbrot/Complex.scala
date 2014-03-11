package hu.nemzsom.mandelbrot

import scala.math._
import scala.BigDecimal

trait Scale {
  def asDouble: Double
  def asBigDec: BigDecimal

  def /(factor: Double): Scale
}

object Scale {
  def apply(s: Double): ScaleAsDouble = new ScaleAsDouble(s)
}

class ScaleAsDouble(s: Double) extends Scale {

  // TODO switch to BigDecimal when needed
  override def /(factor: Double): Scale = new ScaleAsDouble(s / factor)

  override lazy val asBigDec: BigDecimal = BigDecimal(s)

  override val asDouble: Double = s
}

class ScaleAsBigDecimal(s: BigDecimal) extends Scale {

  // TODO switch to Double when can
  override def /(factor: Double): Scale = new ScaleAsBigDecimal(s / factor)

  override lazy val asBigDec: BigDecimal =s

  override val asDouble: Double = s.toDouble
}

sealed trait Complex {

  def re_asDouble: Double
  def im_asDouble: Double

  def re_asBigDec: BigDecimal
  def im_asBigDec: BigDecimal

  def copy: Complex

  def zero: Complex

  def plus(c: Complex): Unit
  def minus(c: Complex): Unit
  def times(c: Complex): Unit

  def modulo: Double

  def diff(diffRe: Int, diffIm: Int, scale: Scale): Complex

  def escaped: Boolean
  def preCheck_isInside: Boolean
}

/** from http://www.stoyanr.com/2013/02/complex-numbers-in-scala.html
  * mutable version for performance
 */
class ComplexWithDouble(private var re: Double, private var im: Double) extends Complex {

  // Constructors
  def this(re: Double) = this(re, 0)

  def re_asDouble = re
  def im_asDouble = im

  def re_asBigDec = BigDecimal(re)
  def im_asBigDec = BigDecimal(im)

  override def copy = new ComplexWithDouble(re, im)

  override def zero = new ComplexWithDouble(0)

  override def escaped: Boolean = re * re + im * im > 2 * 2

  /** Optimization: Cardioid / bulb checking
    * from http://en.wikipedia.org/wiki/Mandelbrot_set#Cardioid_.2F_bulb_checking
    */
  override def preCheck_isInside: Boolean = {
    val q = pow(re - 0.25, 2) + pow(im, 2)
    q * (q + (re - 0.25)) < pow(im, 2) / 4 ||
      pow(re + 1, 2) + pow(im, 2) < 0.0625
  }

  override def modulo = sqrt(pow(re, 2) + pow(im, 2))

  override def plus(c: Complex): Unit = {
    re = re + c.re_asDouble
    im = im + c.im_asDouble
  }

  override def minus(c: Complex): Unit = {
    re = re - c.re_asDouble
    im = im - c.im_asDouble
  }

  override def times(c: Complex): Unit = {
    val newRe = re * c.re_asDouble - im * c.im_asDouble
    val newIm = im * c.re_asDouble + re * c.im_asDouble
    re = newRe
    im = newIm
  }

  override def diff(diffRe: Int, diffIm: Int, scale: Scale): ComplexWithDouble = {
    new ComplexWithDouble(diffRe * scale.asDouble + re, diffIm * scale.asDouble + im)
  }

  def canEqual(other: Any): Boolean = other.isInstanceOf[ComplexWithDouble]

  override def equals(other: Any): Boolean = other match {
    case that: ComplexWithDouble =>
      (that canEqual this) &&
        re == that.re &&
        im == that.im
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(re, im)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }

  // String representation
  override def toString: String =
    (re, im) match {
      case (0, 1) => "i"
      case (real, 0) => real.toString
      case (0, imag) => imag.toString + "*i"
      case _ => asString
    }

  private def asString =
    re + (if (im < 0) "-" + -im else "+" + im) + "*i"
}

class ComplexWithBigDecimal(private var re: BigDecimal, private var im: BigDecimal) extends Complex {

  import Complex.ComplexWithBigDecimal._
  // Constructors
  def this(re: BigDecimal) = this(re, 0)

  def re_asDouble = re.toDouble
  def im_asDouble = im.toDouble

  def re_asBigDec = re
  def im_asBigDec = im

  override def copy = new ComplexWithBigDecimal(re, im)

  override def zero = new ComplexWithBigDecimal(0)

  override def escaped: Boolean = re * re + im * im > FOUR_BIGDEC

  /** Optimization: Cardioid / bulb checking
    * from http://en.wikipedia.org/wiki/Mandelbrot_set#Cardioid_.2F_bulb_checking
    */
  override def preCheck_isInside: Boolean = {
    val reAsDouble = re_asDouble
    val imAsDouble = im_asDouble
    val q = pow(reAsDouble - 0.25, 2) + pow(imAsDouble, 2)
    q * (q + (reAsDouble - 0.25)) < pow(imAsDouble, 2) / 4 ||
      pow(reAsDouble + 1, 2) + pow(imAsDouble, 2) < 0.0625
  }

  override def modulo = sqrt(pow(re_asDouble, 2) + pow(im_asDouble, 2)) // TODO test with smooth coloring

  override def plus(c: Complex): Unit = {
    re = re + c.re_asBigDec
    im = im + c.im_asBigDec
  }

  override def minus(c: Complex): Unit = {
    re = re - c.re_asBigDec
    im = im - c.im_asBigDec
  }

  override def times(c: Complex): Unit = {
    val newRe = re * c.re_asBigDec - im * c.im_asBigDec
    val newIm = im * c.re_asBigDec + re * c.im_asBigDec
    re = newRe
    im = newIm
  }

  override def diff(diffRe: Int, diffIm: Int, scale: Scale): ComplexWithBigDecimal = {
    new ComplexWithBigDecimal(diffRe * scale.asBigDec + re, diffIm * scale.asBigDec + im)
  }

  def canEqual(other: Any): Boolean = other.isInstanceOf[ComplexWithBigDecimal]

  override def equals(other: Any): Boolean = other match {
    case that: ComplexWithDouble =>
      (that canEqual this) &&
        re == that.re_asBigDec &&
        im == that.im_asBigDec
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(re, im)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }

  // String representation
  override def toString: String =
    (re, im) match {
      case (ZERO_BIGDEC, ONE_BIGDEC) => "i"
      case (real, ZERO_BIGDEC) => real.toString()
      case (ZERO_BIGDEC, imag) => imag.toString() + "*i"
      case _ => asString
    }

  private def asString =
    re.toString + (if (im < 0) "-" + -im else "+" + im) + "*i"
}

object Complex {

  def apply(re: Double) = new ComplexWithDouble(re)
  def apply(re: Double, im: Double) = new ComplexWithDouble(re, im)

  def apply(re: BigDecimal) = new ComplexWithBigDecimal(re)
  def apply(re: BigDecimal, im: BigDecimal) = new ComplexWithBigDecimal(re, im)

  object ComplexWithBigDecimal {

    val FOUR_BIGDEC = BigDecimal(4)

    val ZERO_BIGDEC = BigDecimal(0)

    val ONE_BIGDEC = BigDecimal(1)


  }

}