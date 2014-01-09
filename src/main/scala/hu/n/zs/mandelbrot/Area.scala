package hu.n.zs.mandelbrot

import scala.collection.immutable.Range
import scala.collection.Iterator
import scala.collection.immutable.Iterable
import java.awt.Dimension

case class Point(x: Int, y: Int)

object Point {
  implicit def pair2Point(p: (Int, Int)): Point = new Point(p._1, p._2)
  implicit def point2Dimensions(p: Point): Dimension = new Dimension(p.x, p.y)
  implicit def awtPointToPoint(p: java.awt.Point) = new Point(p.x, p.y)
}

/** Area represents a rectangle both on the graphical and the complex pane.
 */
case class Area(pMin: Point, width: Int, height: Int, cMin: Complex, scale: Double) extends Iterable[(Point, Complex)]{
  
  def iterator: Iterator[(Point, Complex)] = for {
    x <- (0 until width).iterator
    im= x * scale + cMin.im
    y <- (0 until height)
    re = y * scale + cMin.re
  } yield (Point(x, y), Complex(re, im))
  
  def topLeft: (Point, Complex) = (pMin, cMin)
  def topRight: (Point, Complex) = (Point(pMin.x + width - 1, pMin.y), Complex(cMin.re, cMin.im + (width - 1) * scale))
  def bottomLeft: (Point, Complex) = (Point(pMin.x, pMin.y + height - 1), Complex(cMin.re + (height - 1) * scale, cMin.im))
  def bottomRight: (Point, Complex) = (Point(pMin.x + width - 1, pMin.y + height - 1), Complex(cMin.re + (height - 1) * scale, cMin.im + (width - 1) * scale))
  def complexAt(p: Point): Complex = Complex(cMin.re + (p.y - pMin.y) * scale, cMin.im + (p.x - pMin.x) * scale)
  
  override def toString() = s"$pMin[$width, $height] | $cMin with scale $scale"
  
  def resize(newWidth: Int, newHeight: Int): Area = Area(pMin, newWidth, newHeight, cMin, scale)
  
  def move(diffX: Int, diffY: Int): Area = {
    val newIm = cMin.im - diffX * scale
    val newRe = cMin.re - diffY * scale
    Area(pMin, width , height, Complex(newRe, newIm), scale)
  }
  
  def zoom(factor: Double, at: Point): Area = {
    val newScale = scale / factor
    // imStart + scale * at.x = newImStart + newScale * at.x
    // newImStart = imStart + scale * at.x - newScale * at.x
    // newImStart = imStart + at.x * (scale - newScale)
    val im = cMin.im + at.x * (scale - newScale)
    val re = cMin.re + at.y * (scale - newScale)
    Area(pMin, width, height, Complex(re, im), newScale)
  }
}

object Area {
  
  def initialize(pMin: Point, width: Int, height: Int, cMin: Complex, reMax: Double): Area =
    Area(pMin, width, height, cMin, calcScale(pMin.y, pMin.y + height - 1, cMin.re, reMax))
    
  def calcScale(gFrom: Int, gTo: Int, cFrom: Double, cTo: Double): Double = (cTo - cFrom) / (gTo - gFrom)
}