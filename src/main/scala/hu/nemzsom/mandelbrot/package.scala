package hu.nemzsom

import java.util.concurrent.LinkedBlockingQueue
import scala.swing.Panel

package object mandelbrot {

  private val debugX = -1
  private val debugY = -1

  var debugPointCoords: (Int, Int) = null

  var debugTime = 0L

  val debugQuene = new LinkedBlockingQueue[Any]

  var debugPanel: Panel = null
  var debugPixels: Array[Int] = null

  def setDebugArea(area: Area): Unit = {
    try {
      val point = area.pointAt(debugX, debugY)
      debugPointCoords = (point.x, point.y)
    }
    catch {
      case e: Throwable =>
        println(s"invalid debug point.$e")
        debugPointCoords = (-100000, -1000000)
    }
  }

  def logPoint(msg: String, p: Point): Unit = {
    if (p.x == debugPointCoords._1 && p.y == debugPointCoords._2) {
      println(s"$msg. $p")
    }
  }
  
  def markDebugPoint(pixelData: Array[Int], i: Int, point: Point): Unit = {
      if ((debugPointCoords._1 - 5 to debugPointCoords._1 + 5).contains(point.x) && point.y == debugPointCoords._2 ||
        point.x == debugPointCoords._1 && (debugPointCoords._2 - 5 to debugPointCoords._2 + 5).contains(point.y)) {
        pixelData(i) = 255 << 16
      }
  }

}
