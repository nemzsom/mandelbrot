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

  override def zero = ComplexWithDouble.ZERO

  override def escaped: Boolean = re * re + im * im > 2 * 2

  /** Optimization: Cardioid / bulb checking
    * from http://en.wikipedia.org/wiki/Mandelbrot_set#Cardioid_.2F_bulb_checking
    */
  override def preCheck_isInside: Boolean = {
    val q = pow(re - 0.25, 2) + pow(im, 2)
    q * (q + (re - 0.25)) < pow(im, 2) / 4 ||
      pow(re + 1, 2) + pow(im, 2) < 0.0625
  }

  // Unary operators
  //def unary_+ = this
  /*def unary_- = {
    re = -re
    im = -im
    this
  }*/
  /*def unary_~ = { // conjugate
    im = -im
    this
  }*/
  //def unary_! = sqrt(pow(re, 2) + pow(im, 2)) // modulo
  override def modulo = sqrt(pow(re, 2) + pow(im, 2))

  // Comparison
  //def compare(that: ComplexWithDouble) = !this compare !that

  // Arithmetic operations
  /*def +(c: ComplexWithDouble) = {
    re = re + c.re
    im = im + c.im
    this
  }*/
  override def plus(c: Complex): Unit = {
    re = re + c.re_asDouble
    im = im + c.im_asDouble
  }
  override def minus(c: Complex): Unit = {
    re = re - c.re_asDouble
    im = im - c.im_asDouble
  }
  /*def -(c: ComplexWithDouble) = {
    re = re - c.re
    im = im - c.im
    this
  }*/
  /*def *(c: ComplexWithDouble) = {
    val newRe = re * c.re - im * c.im
    val newIm = im * c.re + re * c.im
    re = newRe
    im = newIm
    this
  }*/
  override def times(c: Complex): Unit = {
    val newRe = re * c.re_asDouble - im * c.im_asDouble
    val newIm = im * c.re_asDouble + re * c.im_asDouble
    re = newRe
    im = newIm
  }
  /*def /(c: ComplexWithDouble) = {
    require(c.re != 0 || c.im != 0)
    val d = pow(c.re, 2) + pow(c.im, 2)
    val newRe = (re * c.re + im * c.im) / d
    val newIm = (im * c.re - re * c.im) / d
    re = newRe
    im = newIm
    this
  }*/

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

object ComplexWithDouble {

  def ZERO = new ComplexWithDouble(0)
}

object Complex {

  def apply(re: Double): ComplexWithDouble = new ComplexWithDouble(re)
  def apply(re: Double, im: Double): ComplexWithDouble = new ComplexWithDouble(re, im)

  // TODO applys for BigDec

}