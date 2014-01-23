package hu.n.zs.mandelbrot

sealed trait PointLoc

/** Point is inside the mandelbrot set */
case object Inside extends PointLoc
/** We don't know at the current iteration */
case object Unsettled extends PointLoc
/** Point is outside the mandelbrot set, escaped at the specified iteration */
case class Outside(iter: Int) extends PointLoc

class Point (val x: Int, val y: Int, val complexValue: Complex, val index: Int) {

  var iter = 0
  var iterValue = Complex.ZERO
  var location: PointLoc = Unsettled

  override def toString: String = {
    s"Point($x, $y, iter: $iter, iterValue: $iterValue, loc: $location)"
  }
}

object Point {

  import java.awt.Dimension

  def apply(x: Int, y: Int, scale: Double, index: Int) = new Point(x, y, complexAt(x, y, scale), index)

  def complexAt(x: Int, y: Int, scale: Double) = Complex(y * scale, x * scale)

  implicit def point2Dimensions(p: Point): Dimension = new Dimension(p.x, p.y)
  implicit def point2AwtPoint(p: Point) = new java.awt.Point(p.x, p.y)
}

class Area(val scale: Double, val data: Array[Point], val lineStride: Int, val startAt: Int, val width: Int, val height: Int) extends Traversable[Point] {

  def topLeft: Point = data(startAt)

  def indexFor(x: Int, y: Int): Int = startAt + x + y * lineStride

  def pointAt(x: Int, y: Int): Point = data(indexFor(x, y))

  def foreach[U](f: Point => U): Unit = {
    for (x <- 0 until width; y <- 0 until height) {
      f(pointAt(x, y))
    }
  }

  def subArea(x: Int, y: Int, width: Int, height: Int): Area = {
    new Area(scale, data, lineStride, indexFor(x, y), width, height)
  }

  def isUniform: Boolean = {
    val loc = topLeft.location
    forall(_.location == loc)
  }

  /**
   * Splits this area into 3 pieces: 2 pieces as left and right, and an 1px wide cut between them
   * @return triple as (left, cut, right)
   */
  def splitVertical: (Area, Area, Area) = {
    val half = width / 2
    val cut = subArea(half, 0, 1, height)
    val left = subArea(0, 0, half, height)
    val right = subArea(half + 1, 0, width - half - 1, height)
    (left, cut, right)
  }

  /**
   * Splits this area into 3 pieces: 2 pieces as top and bottom, and the 1px tall cut between them
   * @return triple as (top, cut, bottom)
   */
  def splitHorizontal: (Area, Area, Area) = {
    val half = height / 2
    val cut = subArea(0, half, width, 1)
    val top = subArea(0, 0, width, half)
    val bottom = subArea(0, half + 1, width, height - half - 1)
    (top, cut, bottom)
  }
}

object Area {

  def apply(topLeft: (Int, Int), scale: Double, width: Int, height: Int): Area = {
    val (topLeftX, topLeftY) = topLeft
    val size = width * height
    val data = new Array[Point](size)
    var i = 0
    for {
      y <- topLeftY until topLeftY + height
      x <- topLeftX until topLeftX + width
    } {
      data(i) = Point(x, y, scale, i)
      i += 1
    }
    new Area(scale, data, width, 0, width, height)
  }

  def apply(topLeftComp: Complex, reInterval: Double, width: Int, height: Int): Area = {
    val scale: Double = reInterval / (height - 1)
    val reMin = topLeftComp.re
    val imMin = topLeftComp.im
    val topLeft = ((imMin / scale).toInt, (reMin / scale).toInt)
    apply(topLeft, scale, width, height)
  }
}


