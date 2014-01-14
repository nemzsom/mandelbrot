package hu.n.zs.mandelbrot

import PointLoc._
import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class AreaTest extends FunSuite {

  val scale = 1.0
  /**
   *  x,y:                          complexes:
   *  -----                         ------------
   *  (0,0) | (1,0) | (2,0)   |   (0,0) | (0,1) | (0,2)
   *  (0,1) | (1,1) | (2,1)   |   (1,0) | (1,1) | (1,2)
   *  (0,2) | (1,2) | (2,2)   |   (2,0) | (2,1) | (2,2)
   */
  val data = Array(new Point(0, 0, scale, 0, Complex(0, 0), UNSETTLED),
                   new Point(1, 0, scale, 0, Complex(0, 1), UNSETTLED),
                   new Point(2, 0, scale, 0, Complex(0, 2), UNSETTLED),
                   new Point(0, 1, scale, 0, Complex(1, 0), UNSETTLED),
                   new Point(1, 1, scale, 0, Complex(1, 1), UNSETTLED),
                   new Point(2, 1, scale, 0, Complex(1, 2), UNSETTLED),
                   new Point(0, 2, scale, 0, Complex(2, 0), UNSETTLED),
                   new Point(1, 2, scale, 0, Complex(2, 1), UNSETTLED),
                   new Point(2, 2, scale, 0, Complex(2, 2), UNSETTLED)
  )
  val area: Area = new Area(scale, data, 3, 0, 3, 3)

  test("Point should calculate the complex value based on x,y and scale") {
    var complex = new Point(0, 0, 1.0, 0, Complex(0, 0), UNSETTLED).complexValue
    assert(Complex.ZERO === complex)
    complex = new Point(2, 3, 1.0, 0, Complex(0, 0), UNSETTLED).complexValue
    assert(Complex(3, 2) === complex)
    complex = new Point(2, 3, 0.5, 0, Complex(0, 0), UNSETTLED).complexValue
    assert(Complex(1.5, 1) === complex)
  }

  test("topLeft") {
    assert(area.topLeft === new Point(0, 0, scale, 0, Complex(0, 0), UNSETTLED))
    val area2 = new Area(scale, data, 3, 4, 2, 2)
    assert(area2.topLeft === new Point(1, 1, scale, 0, Complex(1, 1), UNSETTLED))
  }

  test("pointAt") {
    for (x <- 0 until area.width; y <- 0 until area.height) {
      val point = area.pointAt(x, y)
      assert(point.x === x, "x should match")
      assert(point.y === y, "y should match")
    }
  }

  test("update") {
    area update { point =>
      val x = point.x
      val y = point.y
      point.iter = x + y
      point.iterValue = point.iterValue * 2
      (point.x, point.y) match {
        case (x, y) if x == y => point.location = UNSETTLED
        case (x, y) if x > y => point.location = INSIDE
        case _ => point.location = OUTSIDE
      }
    }
    assert(data.forall { point =>
      point.iter == point.x + point.y
    }, "")
    //
  }

}
