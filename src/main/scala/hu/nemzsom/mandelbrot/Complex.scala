package hu.nemzsom.mandelbrot

import scala.math._
import java.math.MathContext

/** from http://www.stoyanr.com/2013/02/complex-numbers-in-scala.html 
 */
case class Complex(re: BigDecimal, im: BigDecimal) extends Ordered[Complex] {

  // Constructors
  def this(re: BigDecimal) = this(re, 0)

  // Unary operators
  def unary_+ = this
  def unary_- = new Complex(-re, -im)
  def unary_~ = new Complex(re, -im) // conjugate
  def unary_! = (re.pow(2) + im.pow(2)).sqrt // modulo

  // Comparison
  def compare(that: Complex) = !this compare !that

  // Arithmetic operations
  def +(c: Complex) = new Complex(re + c.re, im + c.im)
  def -(c: Complex) = this + -c
  def *(c: Complex) =
    new Complex(re * c.re - im * c.im, im * c.re + re * c.im)
  def /(c: Complex) = {
    require(c.re != BigDecValues.ZERO || c.im != BigDecValues.ZERO)
    val d = c.re.pow(2) + c.im.pow(2)
    new Complex((re * c.re + im * c.im) / d, (im * c.re - re * c.im) / d)
  }

  // String representation
  override def toString: String =
    this match {
      case Complex.i => "i"
      case Complex(real, BigDecValues.ZERO) => real.toString()
      case Complex(BigDecValues.ZERO, imag) => imag.toString() + "*i"
      case _ => asString
    }
  private def asString: String =
    re.toString + (if (im < 0) "-" + -im else "+" + im) + "*i"

  implicit class BigDecimalOps(val bd: BigDecimal) {

    import BigDecValues._

    // TODO Handle oscillations
    def sqrt: BigDecimal = {
      var guess = bd / TWO
      var done = false
      val maxIterations = bd.mc.getPrecision + 1
      var i = 0
      var result: BigDecimal = null
      while (!done && i < maxIterations) {
        result = (bd / guess + guess) / TWO
        done = result == guess
        guess = result
        i += 1
      }
      guess
    }
  }
}

object Complex {
  // Constants
  val i = new Complex(0, 1)
  val ZERO = new Complex(BigDecimal(0, new MathContext(50)), BigDecimal(0, new MathContext(50)))

  // Factory methods
  def apply(re: Double) = new Complex(re)

  // Implicit conversions
  implicit def fromDouble(d: Double) = new Complex(d)
  implicit def fromFloat(f: Float) = new Complex(f)
  implicit def fromLong(l: Long) = new Complex(l)
  implicit def fromInt(i: Int) = new Complex(i)
  implicit def fromShort(s: Short) = new Complex(s)
}

object BigDecValues {
  val ZERO = BigDecimal(0)
  val TWO = BigDecimal(2)
}