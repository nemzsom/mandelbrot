package hu.nemzsom.mandelbrot

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.junit.Assert

@RunWith(classOf[JUnitRunner])
class AreaTest extends FunSuite {

  /**
   *    x  0       1       2
   *  y ----------------------
   *  0 |(0,0) | (0,1) | (0,2)
   *  1 |(1,0) | (1,1) | (1,2)
   *  2 |(2,0) | (2,1) | (2,2)
   */
  trait TestArea {
    val scale = 1.0
    val area: Area = Area(Complex.ZERO, scale, 3, 3)
    val data = area.data
  }

  test("apply with scale") {
    val scale = 1.0
    val area = Area(Complex(1, 10), scale, 3, 2)
    area.data.zipWithIndex.forall{ case (point, index) =>
      point.index == index
    }
    assert(area.pointAt(0, 0).complexValue === Complex(1, 10))
    assert(area.pointAt(2, 0).complexValue === Complex(1, 12))
    assert(area.pointAt(0, 1).complexValue === Complex(2, 10))
    assert(area.pointAt(2, 1).complexValue === Complex(2, 12))
  }

  test("topLeft") {
    new TestArea {
      assert(area.topLeft.complexValue === Complex(0, 0))
      val area2 = new Area(scale, data, 3, 4, 2, 2)
      assert(area2.topLeft.complexValue === Complex(1, 1))
    }
  }

  test("pointAt") {
    new TestArea {
      for (x <- 0 until area.width; y <- 0 until area.height) {
        val point = area.pointAt(x, y)
        assert(point.complexValue === Complex(y, x))
      }
    }
  }

  test("width and height at complex pane") {
    val area = Area(Complex(-2, -2), 2.0 / 50, 51, 101)
    Assert.assertEquals(2.0, area.mathematicalWidth, 0.001)
    Assert.assertEquals(4.0, area.mathematicalHeight, 0.001)
  }

  test("foreach") {
    new TestArea {
      area foreach  { point =>
        point.iter = point.index
        point.iterValue =  point.complexValue * 2
        point.iter match {
          case multiplyOf10 if multiplyOf10 % 10 == 0 => point.location = Unsettled
          case even if even % 2 == 0 => point.location = Inside
          case odd => point.location = Outside(point.iter)
        }
      }
      assert(data.forall { point =>
        point.iter == point.index &&
        point.iterValue == point.complexValue * 2 &&
        (point.location match {
          case Unsettled => point.iter % 10 == 0
          case Inside => point.iter % 2 == 0
          case Outside(xy) => point.iter % 2 == 1 && point.iter == xy
        })
      })
    }
  }

  test("subArea square") {
    new TestArea {
      val subArea = area.subArea(1, 1, 2, 2)
      assert(subArea.pointAt(0, 0).complexValue == Complex(1, 1))
      assert(subArea.pointAt(1, 0).complexValue == Complex(1, 2))
      assert(subArea.pointAt(0, 1).complexValue == Complex(2, 1))
      assert(subArea.pointAt(1, 1).complexValue == Complex(2, 2))
    }
  }

  test("subArea rectangle") {
    new TestArea {
      val subArea = area.subArea(1, 0, 2, 3)
      assert(subArea.pointAt(0, 0).complexValue == Complex(0, 1))
      assert(subArea.pointAt(1, 0).complexValue == Complex(0, 2))
      assert(subArea.pointAt(0, 2).complexValue == Complex(2, 1))
      assert(subArea.pointAt(1, 2).complexValue == Complex(2, 2))
    }
  }

  test("subArea line") {
    new TestArea {
      val subArea = area.subArea(0, 2, 3, 1)
      assert(subArea.pointAt(0, 0).complexValue == Complex(2, 0))
      assert(subArea.pointAt(1, 0).complexValue == Complex(2, 1))
      assert(subArea.pointAt(2, 0).complexValue == Complex(2, 2))
    }
  }

  test("split vertical with even width") {
    val area: Area = Area(Complex(0, 0), 1, 2, 1)
    val (left, right) = area.splitVertical
    assert(Seq(left, right).forall(a => a.width == 1 && a.height == 1))
    assert(left.topLeft.index === 0)
    assert(right.topLeft.index === 1)
  }

  test("split vertical with odd width") {
    val area: Area = Area(Complex(0, 0), 1, 3, 1)
    val (left, right) = area.splitVertical
    assert(Seq(left, right).forall(a => a.height == 1))
    assert(Seq(left, right).map(_.width) === Seq(1, 2))
    assert(left.topLeft.index === 0)
    assert(right.map(_.index) === Seq(1, 2))
  }

  test("split horizontal with even width") {
    val area: Area = Area(Complex(0, 0), 1, 1, 2)
    val (top, bottom) = area.splitHorizontal
    assert(Seq(top, bottom).forall(a => a.width == 1 && a.height == 1))
    assert(top.topLeft.index === 0)
    assert(bottom.topLeft.index === 1)
  }

  test("split horizontal with odd width") {
    val area: Area = Area(Complex(0, 0), 1, 1, 3)
    val (top, bottom) = area.splitHorizontal
    assert(Seq(top, bottom).forall(a => a.width == 1))
    assert(Seq(top, bottom).map(_.height) === Seq(1, 2))
    assert(top.topLeft.index === 0)
    assert(bottom.map(_.index) === Seq(1, 2))
  }

  test("borders") {
    new TestArea {
      var borderIndexes = area.border map (_.index)
      assert(borderIndexes.size === borderIndexes.toSet.size) // no duplicates
      assert(borderIndexes.toSet === Set(0, 1, 2, 3, 5, 6, 7, 8))
    }
  }

  test("resize") {
    new TestArea {
      testResize(area, 2, 2) // shrink
      testResize(area, 4, 4) // stretch
      testResize(area, 2, 4) // reshape
      testResize(area, 4, 2) // reshape
    }

    def testResize(area: Area, newWidth: Int, newHeight: Int): Unit = {
      val resized = area.resize(newWidth, newHeight)
      assert(resized.width === newWidth)
      assert(resized.height === newHeight)
      var i = 0
      for {
        y <- 0 until newHeight
        x <- 0 until newWidth
      } {
        val p = resized.pointAt(x, y)
        assert(p.complexValue == Complex(y, x))
        assert(p.index === i)
        i += 1
      }
    }
  }
}
