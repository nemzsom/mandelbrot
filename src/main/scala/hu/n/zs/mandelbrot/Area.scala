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
    val xStep = x - pMin.x
    y <- (pMin.y to pMax.y)
    val yStep = y - pMin.y
  } yield (Point(x, y), cMin + Complex(yStep * scale, xStep * scale))
  
  lazy val width = pMax.x - pMin.x + 1
  lazy val height = pMax.y - pMin.y + 1
  
  override def toString() = s"$pMin - $pMax | $cMin with scale $scale"
}

object Area {
  
  def initialize(pMin: Point, pMax: Point, cMin: Complex, reMax: Double): Area =
    Area(pMin, pMax, cMin, calcScale(pMin.y, pMax.y, cMin.re, reMax))
    
  def calcScale(gFrom: Int, gTo: Int, cFrom: Double, cTo: Double) = (cTo - cFrom) / (gTo - gFrom)
}