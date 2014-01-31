package hu.nemzsom.mandelbrot.sandbox

class ColorMapSb {

  def print(arr: Array[Int], msg: String = "Array"): Unit = {
    println(s"$msg: size: ${arr.size}")
    arr.foreach(println(_))
    println("-------------------------------")
  }
  def p(n: Int): String = {
    f"${Integer.toBinaryString(n)}%32s" replace(' ', '0')
  }
}
