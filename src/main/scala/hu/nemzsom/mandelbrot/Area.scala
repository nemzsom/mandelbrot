package hu.nemzsom.mandelbrot

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
    s"Point($x, $y, complexValue: $complexValue, iter: $iter, iterValue: $iterValue, loc: $location, index: $index)"
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

  def foreach[U](f: (Point) => U): Unit =
    for (x <- 0 until width; y <- 0 until height) {
      f(pointAt(x, y))
    }

  def borders: Traversable[Point] = new Traversable[Point] {
    override def foreach[U](f: (Point) => U): Unit = {
      for (x <- 0 until width) { f(pointAt(x, 0)); f(pointAt(x, height - 1)) }
      for (y <- 1 until height - 1) { f(pointAt(0, y)); f(pointAt(width - 1, y)) }
    }
  }

  def subArea(x: Int, y: Int, width: Int, height: Int): Area = {
    new Area(scale, data, lineStride, indexFor(x, y), width, height)
  }

  /**
   * Splits this area into 2 pieces among the longer side.
   * @return tuple as (left, right) or (top, bottom)
   */
  def split: (Area, Area) = {
    if (width > height) { // split horizontal
      val half = width / 2
      val left = subArea(0, 0, half, height)
      val right = subArea(half, 0, width - half, height)
      (left, right)
    }
    else { // split vertical
      val half = height / 2
      val top = subArea(0, 0, width, half)
      val bottom = subArea(0, half, width, height - half)
      (top, bottom)
    }
  }

  override def size: Int = width * height

  override def toString: String = s"Area($topLeft, width: $width, height: $height)"
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


