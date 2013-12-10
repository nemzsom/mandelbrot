package hu.n.zs.mandelbrot

import scala.collection.immutable.Range
import scala.collection.Iterator
import scala.collection.immutable.Iterable
import java.awt.Dimension

case class Point(x: Int, y: Int)

object Point {
  implicit def pair2Point(p: (Int, Int)): Point = new Point(p._1, p._2)
  implicit def point2Dimensions(p: Point): Dimension = new Dimension(p.x, p.y)
}

/** Area represents a rectangle both on the graphical and the complex pane.
 */
case class Area(pMin: Point, pMax: Point, cMin: Complex, scale: Double) extends Iterable[(Point, Complex)]{
  
  def iterator: Iterator[(Point, Complex)] = for {
    x <- (pMin.x to pMax.x).iterator
    val im= (x - pMin.x) * scale + cMin.im
    y <- (pMin.y to pMax.y)
    val re = (y - pMin.y) * scale + cMin.re
  } yield (Point(x, y), Complex(re, im))
  
  lazy val width = pMax.x - pMin.x + 1
  lazy val height = pMax.y - pMin.y + 1
  
  def topLeft: (Point, Complex) = (pMin, cMin)
  def topRight: (Point, Complex) = (Point(pMax.x, pMin.y), Complex(cMin.re, cMin.im + (width - 1) * scale))
  def bottomLeft: (Point, Complex) = (Point(pMin.x, pMax.y), Complex(cMin.re + (height - 1) * scale, cMin.im))
  def bottomRight: (Point, Complex) = (pMax, Complex(cMin.re + (height - 1) * scale, cMin.im + (width - 1) * scale))
  
  override def toString() = s"$pMin - $pMax | $cMin with scale $scale"
  
  def resize(pNewMax: Point): Area = Area(pMin, pNewMax, cMin, scale)
}

object Area {
  
  def initialize(pMin: Point, pMax: Point, cMin: Complex, reMax: Double): Area =
    Area(pMin, pMax, cMin, calcScale(pMin.y, pMax.y, cMin.re, reMax))
    
  def calcScale(gFrom: Int, gTo: Int, cFrom: Double, cTo: Double): Double = (cTo - cFrom) / (gTo - gFrom)
}