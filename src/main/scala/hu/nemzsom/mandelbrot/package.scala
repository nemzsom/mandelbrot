package hu.nemzsom

import java.util.concurrent.LinkedBlockingQueue
import scala.swing.Panel

package object mandelbrot {

  var debugTime = 0L

  val debugQuene = new LinkedBlockingQueue[Any]
}
