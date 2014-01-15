package hu.n.zs.mandelbrot

object PointLoc extends Enumeration {
  type PointLoc = Value
  /** Point is inside the mandelbrot set */
  val INSIDE = Value("INSIDE")
  /** We don't know at the current iteration */
  val UNSETTLED = Value("UNSETTLED")
  /** Point is outside the mandelbrot set */
  val OUTSIDE = Value("OUTSIDE")
}

import PointLoc._
import java.awt.Dimension

class Point (val x: Int, val y: Int, var iter: Int, var iterValue: Complex, var location: PointLoc) {

  def canEqual(other: Any): Boolean = other.isInstanceOf[Point]

  override def equals(other: Any): Boolean = other match {
    case that: Point =>
      (that canEqual this) &&
        x == that.x &&
        y == that.y &&
        iter == that.iter &&
        iterValue == that.iterValue
    case _ => false
  }

  override def hashCode: Int = {
    val state = Seq(x, y, iter, iterValue)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }

  override def toString: String = {
    s"Point($x, $y, iter: $iter, iterValue: $iterValue, loc: $location)"
  }
}

object Point {

  def complexAt(x: Int, y: Int, scale: Double) = Complex(y * scale, x * scale)

  def apply(x: Int, y: Int, scale: Double) = new Point(x, y, 0, complexAt(x, y, scale), UNSETTLED)

  implicit def point2Dimensions(p: Point): Dimension = new Dimension(p.x, p.y)
  implicit def point2AwtPoint(p: Point) = new java.awt.Point(p.x, p.y)
}

class Area(val scale: Double, val data: Array[Point], val lineStride: Int, val startAt: Int, val width: Int, val height: Int) {

  def topLeft: Point = data(startAt)

  def indexFor(x: Int, y: Int): Int = startAt + x + y * lineStride

  def pointAt(x: Int, y: Int): Point = data(indexFor(x, y))

  def update(f: Point => Unit): Unit = {
    for (x <- 0 until width; y <- 0 until height) {
      f(pointAt(x, y))
    }
  }

  def subArea(x: Int, y: Int, width: Int, height: Int): Area = {
    new Area(scale, data, lineStride, indexFor(x, y), width, height)
  }
}

object Area {

  def apply(topLeft: Point, scale: Double, width: Int, height: Int): Area = {
    val size = width * height
    val data = new Array[Point](size)
    var i = 0
    for {
      y <- topLeft.y until topLeft.y + height
      x <- topLeft.x until topLeft.x + width
    } {
      data(i) = Point(x, y, scale)
      i += 1
    }
    new Area(scale, data, width, 0, width, height)
  }
}


