package hu.n.zs.mandelbrot

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class MandelbrotTest extends FunSuite {
  
  val mandelbrot = Mandelbrot(2)
  
  test("0+0*i is inside the set") {
    assert(mandelbrot(new Complex(0,0)) === 2)
  }
  
  test("2+1*i is outside the set") {
    assert(mandelbrot(new Complex(2,1)) < 2)
  }

}