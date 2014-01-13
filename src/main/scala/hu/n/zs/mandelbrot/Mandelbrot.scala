package hu.n.zs.mandelbrot

import math.pow
import scala.annotation.tailrec

object Mandelbrot {
  type Mb = Complex => Int
  
  def contains(iter: Int, c: Complex): Int = {
    if (preCheck(c)) iter
    else iterate(iter, c)
  }
  
  /** Optimization: Cardioid / bulb checking
   *  from http://en.wikipedia.org/wiki/Mandelbrot_set#Cardioid_.2F_bulb_checking
   */
  def preCheck(c: Complex): Boolean = {
    val q = pow(c.re - 0.25, 2) + pow(c.im, 2)
    q*(q + (c.re - 0.25)) < pow(c.im, 2) / 4 ||
    pow(c.re + 1, 2) + pow(c.im, 2) < 0.0625
  }
  
  def iterate(iter: Int, c: Complex): Int = {
    @tailrec def loop(i: Int, z: Complex): Int =
      if (i == iter || z.escaped) i
      else loop(i+1, z*z + c)
    loop(0, Complex.ZERO)
  }
  
  
  
  def apply(iter: Int): Mb = contains(iter, _)
  
  implicit class ComplexOps(c: Complex) {

    def escaped: Boolean = c.re*c.re + c.im*c.im > 2*2
  }
}