package hu.n.zs.mandelbrot

trait ColorMap {

  import Util._

  val nOfColors: Int = 2

  def color(iter: Int): Int = {
    if (iter % 2 == 0) 0 else 0xFFFFFFFF
  }

  lazy val colorMap: Array[Int] = {
    require(nOfColors % 3 == 0)
    val third = nOfColors / 3
    val r, g, b = new Array[Int](third)

    def fill(arr: Array[Int], mask: Int, shift: Int): Unit = {
      val scale = 255 / arr.size
      for (i <- 0 until arr.size) {
        arr(i) = mask | i * scale << shift
      }
    }

    Seq(
      (r, 0x00000000, 16),
      (g, 0x00FF0000, 8),
      (b, 0x00FFFF00, 0)
    ).foreach { case (arr, mask, shift) => fill(arr, mask, shift) }

    r ++ g ++ b :+ 0
  }
}

object Test extends App {
  val cm = new ColorMap {}
  Util.print(cm.colorMap)
}

object Util {

  def print(arr: Array[Int], msg: String = "Array"): Unit = {
    println(s"$msg: size: ${arr.size}")
    arr.foreach(println(_))
    println("-------------------------------")
  }
  def p(n: Int): String = {
    f"${Integer.toBinaryString(n)}%32s" replace(' ', '0')
  }
}
