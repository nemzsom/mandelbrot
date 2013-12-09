package hu.n.zs.mandelbrot

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class AreaTest extends FunSuite {
  
  val area = Area(Point(0, 0), Point(1, 2), Complex(0, 0), 0.5)

  test("iterator") {
    val points = List(
      (Point(0, 0), Complex(0, 0)),
      (Point(0, 1), Complex(0.5, 0)),
      (Point(0, 2), Complex(1, 0)),
      (Point(1, 0), Complex(0, 0.5)),
      (Point(1, 1), Complex(0.5, 0.5)),
      (Point(1, 2), Complex(1, 0.5)))
    assert(points === area.toList)
  }
  
  test("initialize") {
    val a = Area.initialize(Point(0, 0), Point(1, 2), Complex(0, 0), 1)
    assert(a === area)
  }
  
  test("width") {
    assert(area.width === 2)
  }
  
  test("height") {
    assert(area.height === 3)
  }
  
  test("calcScale") {
    val scale = Area.calcScale(0, 10, 0, 1)
    assert(scale === 0.1)
  }

}