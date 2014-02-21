package hu.nemzsom.mandelbrot

import scala.math._

/** from http://www.stoyanr.com/2013/02/complex-numbers-in-scala.html 
 */
case class Complex(re: Double, im: Double) extends Ordered[Complex] {

  // Constructors
  def this(re: Double) = this(re, 0)

  // Unary operators
  def unary_+ = this
  def unary_- = new Complex(-re, -im)
  def unary_~ = new Complex(re, -im) // conjugate
  def unary_! = sqrt(pow(re, 2) + pow(im, 2))

  // Comparison
  def compare(that: Complex) = !this compare !that

  // Arithmetic operations
  def +(c: Complex) = new Complex(re + c.re, im + c.im)
  def -(c: Complex) = this + -c
  def *(c: Complex) =
    new Complex(re * c.re - im * c.im, im * c.re + re * c.im)
  def /(c: Complex) = {
    require(c.re != 0 || c.im != 0)
    val d = pow(c.re, 2) + pow(c.im, 2)
    new Complex((re * c.re + im * c.im) / d, (im * c.re - re * c.im) / d)
  }

  // String representation
  override def toString: String =
    this match {
      case Complex.i => "i"
      case Complex(real, 0) => real.toString
      case Complex(0, imag) => imag.toString + "*i"
      case _ => asString
    }
  private def asString =
    re + (if (im < 0) "-" + -im else "+" + im) + "*i"
}

object Complex {
  // Constants
  val i = new Complex(0, 1)
  val ZERO = new Complex(0, 0)

  // Factory methods
  def apply(re: Double) = new Complex(re)

  // Implicit conversions
  implicit def fromDouble(d: Double) = new Complex(d)
  implicit def fromFloat(f: Float) = new Complex(f)
  implicit def fromLong(l: Long) = new Complex(l)
  implicit def fromInt(i: Int) = new Complex(i)
  implicit def fromShort(s: Short) = new Complex(s)
}