package hu.n.zs.mandelbrot

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
    val area: Area = Area((0, 0), scale, 3, 3)
    val data = area.data
  }

  def assertCoords(point: Point, coords: (Int, Int)) = {
    val (x, y) = coords
    assert(point.x === x)
    assert(point.y === y)
  }

  def assertPoint(point: Point, coords: (Int, Int), scale: Double) = {
    assertCoords(point, coords)
    assert(point.complexValue === Point.complexAt(coords._1, coords._2, scale))
  }

  test("Point.complexAt should calculate the complex value based on x,y and scale") {
    assert(Point.complexAt(0, 0, 1.0) === Complex.ZERO)
    assert(Point.complexAt(2, 3, 1.0) === Complex(3, 2))
    assert(Point.complexAt(2, 3, 0.5) === Complex(1.5, 1))
  }

  test("apply with scale") {
    val scale = 1.0
    val area = Area((2, 3), scale, 3, 2)
    area.data.zipWithIndex.forall{ case (point, index) =>
      point.index == index
    }
    assertCoords(area.pointAt(0, 0), (2, 3))
    assertCoords(area.pointAt(2, 0), (4, 3))
    assertCoords(area.pointAt(0, 1), (2, 4))
    assertCoords(area.pointAt(2, 1), (4, 4))
  }

  test("apply with complex specifications") {
    val area = Area(Complex(-1, -1), 2, 5, 9)
    area.data.zipWithIndex.forall{ case (point, index) =>
      point.index == index
    }
    val expectedScale = 0.25
    assert(area.scale === expectedScale)
    assertPoint(area.topLeft, (-4, -4), expectedScale)
    assertPoint(area.data.last, (0, 4), expectedScale)
    assertPoint(area.pointAt(4, 4), (0, 0), expectedScale)
  }

  test("topLeft") {
    new TestArea {
      assertCoords(area.topLeft, (0, 0))
      val area2 = new Area(scale, data, 3, 4, 2, 2)
      assertCoords(area2.topLeft, (1, 1))
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

  test("foreach") {
    new TestArea {
      area foreach  { point =>
        val px = point.x
        val py = point.y
        point.iter = px + py
        point.iterValue =  point.complexValue * 2
        (point.x, point.y) match {
          case (x, y) if x == y => point.location = Unsettled
          case (x, y) if x > y => point.location = Inside
          case (x, y) => point.location = Outside(x + y)
        }
      }

      assert(data.forall { point =>
        point.iter == point.x + point.y &&
        point.iterValue == point.complexValue * 2 &&
        (point.location match {
          case Unsettled => point.x == point.y
          case Inside => point.x > point.y
          case Outside(xy) => point.x < point.y && xy == point.x + point.y
        })
      })
    }
  }

  test("subArea square") {
    new TestArea {
      val subArea = area.subArea(1, 1, 2, 2)
      assertCoords(subArea.pointAt(0, 0), (1, 1))
      assertCoords(subArea.pointAt(1, 0), (2, 1))
      assertCoords(subArea.pointAt(0, 1), (1, 2))
      assertCoords(subArea.pointAt(1, 1), (2, 2))
    }
  }

  test("subArea rectangle") {
    new TestArea {
      val subArea = area.subArea(1, 0, 2, 3)
      assertCoords(subArea.pointAt(0, 0), (1, 0))
      assertCoords(subArea.pointAt(1, 0), (2, 0))
      assertCoords(subArea.pointAt(0, 2), (1, 2))
      assertCoords(subArea.pointAt(1, 2), (2, 2))
    }
  }

  test("subArea line") {
    new TestArea {
      val subArea = area.subArea(0, 2, 3, 1)
      assertCoords(subArea.pointAt(0, 0), (0, 2))
      assertCoords(subArea.pointAt(1, 0), (1, 2))
      assertCoords(subArea.pointAt(2, 0), (2, 2))
    }
  }

  test("split horizontal with odd height") {
    val area: Area = Area((0, 0), 1, 10, 5)
    val (top, cut, bottom) = area.splitHorizontal
    assert(Seq(top, cut, bottom).forall(_.width == 10))
    assert(top.height === 2)
    assert(cut.height === 1)
    assert(bottom.height === 2)
    assert(top.map(_.index).toSet == (0 until 20).toSet)
    assert(cut.map(_.index).toSet == (20 until 30).toSet)
    assert(bottom.map(_.index).toSet == (30 until 50).toSet)
  }

  test("split horizontal with even height") {
    val area: Area = Area((0, 0), 1, 10, 4)
    val (top, cut, bottom) = area.splitHorizontal
    assert(Seq(top, cut, bottom).forall(_.width == 10))
    assert(top.height === 2)
    assert(cut.height === 1)
    assert(bottom.height === 1)
    assert(top.map(_.index).toSet == (0 until 20).toSet)
    assert(cut.map(_.index).toSet == (20 until 30).toSet)
    assert(bottom.map(_.index).toSet == (30 until 40).toSet)
  }

  test("split vertical with odd width") {
    val area: Area = Area((0, 0), 1, 5, 10)
    val (right, cut, left) = area.splitVertical
    assert(Seq(right, cut, left).forall(_.height == 10))
    assert(right.width === 2)
    assert(cut.width === 1)
    assert(left.width === 2)
    assert(right.map(_.index).toSet === (for (x <- (0 until 2); y <- (0 until 10)) yield(y * 5 + x)).toSet)
    assert(cut.map(_.index).toSet === (for (y <- (0 until 10)) yield(y * 5 + 2)).toSet)
    assert(left.map(_.index).toSet === (for (x <- (3 until 5); y <- (0 until 10)) yield(y * 5 + x)).toSet)
  }

  test("split vertical with even width") {
    val area: Area = Area((0, 0), 1, 4, 10)
    val (right, cut, left) = area.splitVertical
    assert(Seq(right, cut, left).forall(_.height == 10))
    assert(right.width === 2)
    assert(cut.width === 1)
    assert(left.width === 1)
    assert(right.map(_.index).toSet === (for (x <- (0 until 2); y <- (0 until 10)) yield(y * 4 + x)).toSet)
    assert(cut.map(_.index).toSet === (for (y <- (0 until 10)) yield(y * 4 + 2)).toSet)
    assert(left.map(_.index).toSet === (for (y <- (0 until 10)) yield(y * 4 + 3)).toSet)
  }
}
