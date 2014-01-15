package hu.n.zs.mandelbrot

import PointLoc._
import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class AreaTest extends FunSuite {


  /**
   *  x,y:                          complexes:
   *  -----                         ------------
   *  (0,0) | (1,0) | (2,0)   |   (0,0) | (0,1) | (0,2)
   *  (0,1) | (1,1) | (2,1)   |   (1,0) | (1,1) | (1,2)
   *  (0,2) | (1,2) | (2,2)   |   (2,0) | (2,1) | (2,2)
   */
  trait TestArea {
    val scale = 1.0
    val area: Area = Area(Point(0, 0, scale), scale, 3, 3)
    val data = area.data
  }

  test("Point.complexAt should calculate the complex value based on x,y and scale") {
    assert(Point.complexAt(0, 0, 1.0) === Complex.ZERO)
    assert(Point.complexAt(2, 3, 1.0) === Complex(3, 2))
    assert(Point.complexAt(2, 3, 0.5) === Complex(1.5, 1))
  }

  test("apply") {
    val scale = 1.0
    val area = Area(Point(2, 3, scale), scale, 3, 2)
    assert(area.pointAt(0, 0) === Point(2, 3, scale))
    assert(area.pointAt(2, 0) === Point(4, 3, scale))
    assert(area.pointAt(0, 1) === Point(2, 4, scale))
    assert(area.pointAt(2, 1) === Point(4, 4, scale))
  }

  test("topLeft") {
    new TestArea {
      assert(area.topLeft === Point(0, 0, scale))
      val area2 = new Area(scale, data, 3, 4, 2, 2)
      assert(area2.topLeft === Point(1, 1, scale))
    }
  }

  test("pointAt") {
    new TestArea {
      for (x <- 0 until area.width; y <- 0 until area.height) {
        val point = area.pointAt(x, y)
        assert(point.x === x, "x should match")
        assert(point.y === y, "y should match")
      }
    }
  }

  test("update") {
    new TestArea {
      area update { point =>
        val px = point.x
        val py = point.y
        point.iter = px + py
        point.iterValue =  point.iterValue * 2
        (point.x, point.y) match {
          case (x, y) if x == y => point.location = UNSETTLED
          case (x, y) if x > y => point.location = INSIDE
          case _ => point.location = OUTSIDE
        }
      }
      assert(data.forall { point =>
        point.iter == point.x + point.y &&
        point.iterValue == Point.complexAt(point.x, point.y, scale) * 2 &&
        (point.location match {
          case UNSETTLED => point.x == point.y
          case INSIDE => point.x > point.y
          case OUTSIDE => point.x < point.y
        })
      })
    }
  }

  test("subArea square") {
    new TestArea {
      val subArea = area.subArea(1, 1, 2, 2)
      assert(subArea.pointAt(0, 0) === Point(1, 1, scale))
      assert(subArea.pointAt(1, 0) === Point(2, 1, scale))
      assert(subArea.pointAt(0, 1) === Point(1, 2, scale))
      assert(subArea.pointAt(1, 1) === Point(2, 2, scale))
    }
  }

  test("subArea rectangle") {
    new TestArea {
      val subArea = area.subArea(1, 0, 2, 3)
      assert(subArea.pointAt(0, 0) === Point(1, 0, scale))
      assert(subArea.pointAt(1, 0) === Point(2, 0, scale))
      assert(subArea.pointAt(0, 2) === Point(1, 2, scale))
      assert(subArea.pointAt(1, 2) === Point(2, 2, scale))
    }
  }

  test("subArea line") {
    new TestArea {
      val subArea = area.subArea(0, 2, 3, 1)
      assert(subArea.pointAt(0, 0) === Point(0, 2, scale))
      assert(subArea.pointAt(1, 0) === Point(1, 2, scale))
      assert(subArea.pointAt(2, 0) === Point(2, 2, scale))
    }
  }
}
