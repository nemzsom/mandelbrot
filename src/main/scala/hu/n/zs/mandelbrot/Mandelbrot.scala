package hu.n.zs.mandelbrot

object Mandelbrot {
  type Mb = Complex => Boolean
  
  def contains(i: Int, z: Complex, c: Complex): Boolean = {
      if (i < 0) true
      else if (z.escaped) false
      else contains(i-1, z*z + c, c)
  }
  
  def apply(iter: Int): Mb = contains(iter, Complex(0, 0), _)
  
  implicit class ComplexOps(c: Complex) {

    def escaped: Boolean = math.abs(c.re*c.re + c.im*c.im) > 2*2
  }
}