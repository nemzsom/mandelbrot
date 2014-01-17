package hu.n.zs.mandelbrot

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
@deprecated
class MandelbrotDeprecatedTest extends FunSuite {
  
  val mandelbrot = Mandelbrot_deprecated(2)
  
  test("0+0*i is inside the set") {
    assert(mandelbrot(new Complex(0,0)) === 2)
  }
  
  test("2+1*i is outside the set") {
    assert(mandelbrot(new Complex(2,1)) < 2)
  }

}