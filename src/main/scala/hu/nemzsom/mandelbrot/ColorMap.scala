package hu.nemzsom.mandelbrot

trait ColorMap {

  def color(point: Point): Int
}

class Black_and_WhiteColorMap extends ColorMap {

  def color(point: Point): Int = point.location match {
    case Outside(iter) => if (iter % 2 == 0) 0 else 0xFFFFFFFF
    case _ => 0 // Inside or Unsettled
  }
}

class LinearColorMap(nOfColors: Int) extends ColorMap {

  require(nOfColors % 3 == 0) // TODO allow numbers that isn't divisible by 3

  val colorMap: Array[Int] = {
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

  def color(point: Point): Int = point.location match {
    case Inside => 0
    case Outside(iter) => colorMap(iter % nOfColors)
    case Unsettled => 255
  }
}