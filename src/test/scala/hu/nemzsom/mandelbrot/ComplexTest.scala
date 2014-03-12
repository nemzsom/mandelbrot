package hu.nemzsom.mandelbrot

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import java.math.MathContext


@RunWith(classOf[JUnitRunner])
class ComplexWithBigDecimalTest extends FunSuite{

  test("diff") {
    val c = new ComplexWithBigDecimal(BigDecimal(1, new MathContext(10)), BigDecimal(0, new MathContext(10)))
    val diff = c.diff(10, 10, Scale(0.1))
    assert(diff.re_asBigDec.mc.getPrecision === 10)
    assert(diff.im_asBigDec.mc.getPrecision === 10)
  }

}
