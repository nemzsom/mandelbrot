package hu.n.zs.mandelbrot

import scala.collection.Iterator
import scala.collection.immutable.Iterable
import java.awt.Dimension

@deprecated
case class Point_deprecated(x: Int, y: Int)

@deprecated
object Point_deprecated {
  implicit def pair2Point(p: (Int, Int)): Point_deprecated = new Point_deprecated(p._1, p._2)
  implicit def point2Dimensions(p: Point_deprecated): Dimension = new Dimension(p.x, p.y)
  implicit def awtPointToPoint(p: java.awt.Point) = new Point_deprecated(p.x, p.y)
}

/** Area represents a rectangle both on the graphical and the complex pane.
 */
@deprecated
case class Area_deprecated(pMin: Point_deprecated, width: Int, height: Int, cMin: Complex, scale: Double) extends Iterable[(Point_deprecated, Complex)]{
  
  def iterator: Iterator[(Point_deprecated, Complex)] = for {
    x <- (0 until width).iterator
    im= x * scale + cMin.im
    y <- 0 until height
    re = y * scale + cMin.re
  } yield (Point_deprecated(x, y), Complex(re, im))
  
  def topLeft: (Point_deprecated, Complex) = (pMin, cMin)
  def topRight: (Point_deprecated, Complex) = (Point_deprecated(pMin.x + width - 1, pMin.y), Complex(cMin.re, cMin.im + (width - 1) * scale))
  def bottomLeft: (Point_deprecated, Complex) = (Point_deprecated(pMin.x, pMin.y + height - 1), Complex(cMin.re + (height - 1) * scale, cMin.im))
  def bottomRight: (Point_deprecated, Complex) = (Point_deprecated(pMin.x + width - 1, pMin.y + height - 1), Complex(cMin.re + (height - 1) * scale, cMin.im + (width - 1) * scale))
  def complexAt(p: Point_deprecated): Complex = Complex(cMin.re + (p.y - pMin.y) * scale, cMin.im + (p.x - pMin.x) * scale)
  
  override def toString() = s"$pMin[$width, $height] | $cMin with scale $scale"
  
  def resize(newWidth: Int, newHeight: Int): Area_deprecated = Area_deprecated(pMin, newWidth, newHeight, cMin, scale)
  
  def move(diffX: Int, diffY: Int): Area_deprecated = {
    val newIm = cMin.im - diffX * scale
    val newRe = cMin.re - diffY * scale
    Area_deprecated(pMin, width , height, Complex(newRe, newIm), scale)
  }
  
  def zoom(factor: Double, at: Point_deprecated): Area_deprecated = {
    val newScale = scale / factor
    val im = cMin.im + at.x * (scale - newScale)
    val re = cMin.re + at.y * (scale - newScale)
    Area_deprecated(pMin, width, height, Complex(re, im), newScale)
  }
}

@deprecated
object Area_deprecated {
  
  def initialize(pMin: Point_deprecated, width: Int, height: Int, cMin: Complex, reMax: Double): Area_deprecated =
    Area_deprecated(pMin, width, height, cMin, calcScale(pMin.y, pMin.y + height - 1, cMin.re, reMax))
    
  def calcScale(gFrom: Int, gTo: Int, cFrom: Double, cTo: Double): Double = (cTo - cFrom) / (gTo - gFrom)
}