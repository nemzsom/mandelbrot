package hu.nemzsom.mandelbrot

import scala.swing.Component

trait Renderer {

  val painter: Component
  val pixels: Array[Int]

  def color(point: Point): Unit = {
    pixels(point.index) = point.location match {
      case Inside => 0
      case Outside(iter) => if (iter % 2 == 0) 0 else 0xFFFFFFFF
      case Unsettled => 255
    }
  }

}
