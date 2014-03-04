package hu.nemzsom.mandelbrot

object Mock {

  object Plotter extends Plotter {
    override def finish(points: Traversable[Point], cancel: () => Boolean): Unit = {}

    override def plot(p: Point): Unit = {}
  }
}
