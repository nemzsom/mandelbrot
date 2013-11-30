package hu.n.zs.mandelbrot

object Mandelbrot {
  type Mb = Complex => Int
  
  def contains(iter: Int, c: Complex): Int = {
    def loop(i: Int, z: Complex): Int =
      if (i == 0 || z.escaped) i
      else loop(i-1, z*z + c)
    loop(iter, Complex(0, 0))
  }
  
  def apply(iter: Int): Mb = contains(iter, _)
  
  implicit class ComplexOps(c: Complex) {

    def escaped: Boolean = math.abs(c.re*c.re + c.im*c.im) > 2*2
  }
}