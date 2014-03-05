package hu.nemzsom.mandelbrot

sealed trait PointLoc

/** Point is inside the mandelbrot set */
case object Inside extends PointLoc
/** We don't know at the current iteration */
case object Unsettled extends PointLoc
/** Point is outside the mandelbrot set, escaped at the specified iteration */
case class Outside(iter: Int) extends PointLoc

class Point(val complexValue: Complex, var index: Int) {

  var iter = 0
  var iterValue = Complex.ZERO
  var location: PointLoc = Unsettled

  override def toString: String = {
    s"Point(complexValue: $complexValue, iter: $iter, iterValue: $iterValue, loc: $location, index: $index)"
  }
}

class Area(val scale: Double, val data: Array[Point], val lineStride: Int, val startAt: Int, val width: Int, val height: Int) extends Traversable[Point] {

  lazy val topLeft: Point = data(startAt)

  def indexFor(x: Int, y: Int): Int = startAt + x + y * lineStride

  def pointAt(x: Int, y: Int): Point = data(indexFor(x, y))

  def foreach[U](f: (Point) => U): Unit =
    for (x <- 0 until width; y <- 0 until height) {
      f(pointAt(x, y))
    }

  def border: Traversable[Point] = new Traversable[Point] {
    override def foreach[U](f: (Point) => U): Unit = {
      for (x <- 0 until width) { f(pointAt(x, 0)); f(pointAt(x, height - 1)) }
      for (y <- 1 until height - 1) { f(pointAt(0, y)); f(pointAt(width - 1, y)) }
    }
  }

  def subArea(x: Int, y: Int, width: Int, height: Int): Area = {
    new Area(scale, data, lineStride, indexFor(x, y), width, height)
  }

  /**
   * Splits this area into 2 pieces horizontally
   * @return tuple of (top, bottom)
   */
  def splitHorizontal: (Area, Area) = {
    val half = height / 2
    val top = subArea(0, 0, width, half)
    val bottom = subArea(0, half, width, height - half)
    (top, bottom)
  }

  /**
   * Splits this area into 2 pieces vertically
   * @return tuple of (left, right)
   */
  def splitVertical: (Area, Area) = {
    val half = width / 2
    val left = subArea(0, 0, half, height)
    val right = subArea(half, 0, width - half, height)
    (left, right)
  }

  def resize(newWidth: Int, newHeight: Int): Area = {
    val newSize = newWidth * newHeight
    val newData = new Array[Point](newSize)
    var i = 0
    for {
      y <- 0 until newHeight
      x <- 0 until newWidth
    } {
      newData(i) = {
        if (x < width && y < height) {
          val p = pointAt(x, y)
          p.index = i // sets the new index
          p
        }
        else new Point(topLeft.complexValue + Complex(y * scale, x * scale), i)
      }
      i += 1
    }
    new Area(scale, newData, newWidth, 0, newWidth, newHeight)
  }

  def move(diffX: Int, diffY: Int): Area = {
    val newData = new Array[Point](size)
    var i = 0
    for {
      y <- 0 until height
      oldY = y - diffY
      x <- 0 until width
      oldX = x - diffX
    } {
      newData(i) = {
        if (oldX >= 0 && oldX < width && oldY >= 0 && oldY < height) {
          val p = pointAt(oldX, oldY)
          p.index = i // sets the new index
          p
        }
        else new Point(topLeft.complexValue + Complex(oldY * scale, oldX * scale), i)
      }
      i += 1
    }
    new Area(scale, newData, width, 0, width, height)
  }

  def zoom(factor: Double, at: (Int, Int)): Area = {
    val (x, y) = at
    val complexAt = pointAt(x, y).complexValue
    val newScale = scale / factor
    val topLeftComplex = complexAt - Complex(newScale * y, newScale * x)
    Area(topLeftComplex, newScale, width, height)
  }

  def mathematicalWidth: BigDecimal =
    pointAt(width - 1, 0).complexValue.im - topLeft.complexValue.im

  def mathematicalHeight: BigDecimal =
    pointAt(0, height - 1).complexValue.re - topLeft.complexValue.re

  override def size: Int = width * height

  override def toString(): String = s"Area($topLeft, width: $width, height: $height, mathWidth: $mathematicalWidth, mathHeight: $mathematicalHeight)"
}

object Area {

  def onePoint(complexValue: Complex, scale: Double): Area = {
    new Area(scale, Array(new Point(complexValue, 0)), 1, 0, 1, 1)
  }

  def apply(topLeftComp: Complex, scale: Double, width: Int, height: Int): Area =
    onePoint(topLeftComp, scale).resize(width, height)
}


