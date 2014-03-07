package hu.nemzsom.mandelbrot

import scala.math._

/** from http://www.stoyanr.com/2013/02/complex-numbers-in-scala.html
  * mutable version for performance
 */
class Complex(var re: Double, var im: Double) extends Ordered[Complex] {

  // Constructors
  def this(re: Double) = this(re, 0)

  // Unary operators
  def unary_+ = this
  def unary_- = {
    re = -re
    im = -im
    this
  }
  def unary_~ = { // conjugate
    im = -im
    this
  }
  def unary_! = sqrt(pow(re, 2) + pow(im, 2)) // modulo

  // Comparison
  def compare(that: Complex) = !this compare !that

  // Arithmetic operations
  def +(c: Complex) = {
    re = re + c.re
    im = im + c.im
    this
  }
  def -(c: Complex) = {
    re = re - c.re
    im = im - c.im
    this
  }
  def *(c: Complex) = /*new Complex(re * c.re - im * c.im, im * c.re + re * c.im)*/{
    val newRe = re * c.re - im * c.im
    val newIm = im * c.re + re * c.im
    re = newRe
    im = newIm
    this
  }
  def /(c: Complex) = {
    require(c.re != 0 || c.im != 0)
    val d = pow(c.re, 2) + pow(c.im, 2)
    val newRe = (re * c.re + im * c.im) / d
    val newIm = (im * c.re - re * c.im) / d
    re = newRe
    im = newIm
    this
  }

  // String representation
  override def toString: String =
    this match {
      /*case Complex(0, 1) => "i"
      case Complex(real, 0) => real.toString
      case Complex(0, imag) => imag.toString + "*i"*/
      case _ => asString
    }
  private def asString =
    re + (if (im < 0) "-" + -im else "+" + im) + "*i"
}

object Complex {

  // Factory methods
  def apply(re: Double) = new Complex(re)
  def apply(re: Double, im: Double) = new Complex(re, im)

  // Implicit conversions
  implicit def fromDouble(d: Double) = new Complex(d)
  implicit def fromFloat(f: Float) = new Complex(f)
  implicit def fromLong(l: Long) = new Complex(l)
  implicit def fromInt(i: Int) = new Complex(i)
  implicit def fromShort(s: Short) = new Complex(s)
}