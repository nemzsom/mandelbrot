package hu.n.zs.mandelbrot

object Mandelbrot {
  type Mb = Complex => Int
  
  def contains(iter: Int, c: Complex): Int = {
    def loop(i: Int, z: Complex): Int =
      if (i == iter || z.escaped) i
      else loop(i+1, z*z + c)
    loop(0, Complex(0, 0))
  }
  
  def apply(iter: Int): Mb = contains(iter, _)
  
  implicit class ComplexOps(c: Complex) {

    def escaped: Boolean = c.re*c.re + c.im*c.im > 2*2
  }
}