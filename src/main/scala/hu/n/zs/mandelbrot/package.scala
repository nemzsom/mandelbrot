package hu.n.zs

package object mandelbrot {

  private val debugX = -1
  private val debugY = -1

  var debugPointAt: Point = null

  def setDebugArea(area: Area): Unit = {
    try {
    debugPointAt = area.pointAt(debugX, debugY)
    }
    catch {
      case e =>
        println(s"invalid debug point.$e")
        debugPointAt = new Point(-100000, -1000000, Complex.ZERO)
    }
  }

  def logPoint(msg: String, p: Point): Unit = {
    if (p.x == debugPointAt.x && p.y == debugPointAt.y) {
      println(s"$msg. $p")
    }
  }

}
