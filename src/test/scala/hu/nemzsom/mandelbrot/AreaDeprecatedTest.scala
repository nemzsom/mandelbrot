package hu.nemzsom.mandelbrot

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
@deprecated
class AreaDeprecatedTest extends FunSuite {
  
  /**
   *     0        1
   *   __________________
   * 0 |0+0i  , 0+0.5i  |
   * 1 |0.5+0i, 0.5+0.5i|
   * 2 |1+0i  , 1+0.5i  |
   *   ------------------
   */
  val area = Area_deprecated(Point_deprecated(0, 0), 2, 3, Complex(0, 0), 0.5)

  test("iterator") {
    val points = List(
      (Point_deprecated(0, 0), Complex(0, 0)),
      (Point_deprecated(0, 1), Complex(0.5, 0)),
      (Point_deprecated(0, 2), Complex(1, 0)),
      (Point_deprecated(1, 0), Complex(0, 0.5)),
      (Point_deprecated(1, 1), Complex(0.5, 0.5)),
      (Point_deprecated(1, 2), Complex(1, 0.5)))
    assert(points === area.toList)
  }
  
  test("initialize") {
    val a = Area_deprecated.initialize(Point_deprecated(0, 0), 2, 3, Complex(0, 0), 1)
    assert(a === area)
  }
  
  test("width") {
    assert(area.width === 2)
  }
  
  test("height") {
    assert(area.height === 3)
  }
  
  test("calcScale") {
    val scale = Area_deprecated.calcScale(0, 10, 0, 1)
    assert(scale === 0.1)
  }
  
  test("complexAt") {
    assert(area.complexAt(1, 2) === Complex(1, 0.5))
  }
  
  test("corners") {
    assert(area.topLeft === (Point_deprecated(0, 0), Complex(0, 0)))
    assert(area.topRight === (Point_deprecated(1, 0), Complex(0, 0.5)))
    assert(area.bottomLeft === (Point_deprecated(0, 2), Complex(1, 0)))
    assert(area.bottomRight === (Point_deprecated(1, 2), Complex(1, 0.5)))
  }
  
  test("resize") {
    val b = area resize (3, 2)
    assert(b.topLeft === (Point_deprecated(0, 0), Complex(0, 0)))
    assert(b.bottomRight === (Point_deprecated(2, 1), Complex(0.5, 1)))
  }
  
  test("move") {
    val moved = area move (1, 2)
    assert(moved.topLeft === (Point_deprecated(0, 0), Complex(-1, -0.5)))
    assert(moved.bottomRight === (Point_deprecated(1, 2), Complex(0, 0)))
  }
  
  test("zoom") {
    val zoomed = area zoom (2, (1, 1))
    assert(zoomed.complexAt(1, 1) === Complex(0.5, 0.5))
    assert(zoomed.topLeft === (Point_deprecated(0, 0), Complex(0.25, 0.25)))
    assert(zoomed.bottomRight === (Point_deprecated(1, 2), Complex(0.75, 0.5)))
  }

}