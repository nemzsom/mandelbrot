package hu.nemzsom.mandelbrot

object Sandbox extends App {

  new CalculatorPTester(4).measure(CalcPTest(640, 480, Complex(-2, -1.65), 2.5, 1000, 100))
}

object Mock {
  object Plotter extends Plotter {
    override def finish(points: Traversable[Point], cancel: () => Boolean): Unit = {}

    override def plot(p: Point): Unit = {}
  }
}
