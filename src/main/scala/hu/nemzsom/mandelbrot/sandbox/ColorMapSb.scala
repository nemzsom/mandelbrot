package hu.nemzsom.mandelbrot.sandbox

import scala.swing.Swing._
import scala.swing.{MainFrame, Panel, SimpleSwingApplication}
import java.awt.image.BufferedImage
import java.awt.{Color, Graphics2D}
import scala.swing.event.{MouseWheelMoved, MousePressed}
import java.awt.event.InputEvent

object ColorMapSb extends SimpleSwingApplication {

  val width = 450
  val height = 50

  lazy val ui = new Panel {

    preferredSize = (width, height)
    var bufferedImage = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
    listenTo(mouse.clicks, mouse.wheel)

    reactions += {
      case MousePressed(_, _, modifiers, _, _) =>
        if ((modifiers & InputEvent.BUTTON1_DOWN_MASK) > 0) saturation = Math.min(saturation + 0.05f, 1.0f)
        if ((modifiers & InputEvent.BUTTON3_DOWN_MASK) > 0) saturation = Math.max(saturation - 0.05f, 0.0f)
        println(s"saturation: $saturation")
        updateColors(HSBCycle)
        repaint()
      case MouseWheelMoved(_, _, _, rotation) =>
        if (rotation > 0) brightness = Math.min(brightness + 0.05f, 1.0f)
        if (rotation < 0) brightness = Math.max(brightness - 0.05f, 0.0f)
        println(s"brightness: $brightness")
        updateColors(HSBCycle)
        repaint()
    }

    override def paintComponent(g: Graphics2D) = {
      super.paintComponent(g)
      for {
        x <- 0 until width
        color = colors(x % colorCount)
        y <- 0 until height
      } {
        bufferedImage.setRGB(x, y, color)
      }
      g.drawImage(bufferedImage, 0, 0, bufferedImage.getWidth, bufferedImage.getHeight, null)
    }
  }

  var saturation = 0.7f
  var brightness = 0.9f
  val colorCount = 450

  val colors = new Array[Int](colorCount)

  def updateColors(map: Int => Int): Unit =
    for (i <- colors.indices) {
      colors(i) = map(i)
    }

  def HSBCycle: Int => Int = { index =>
    Color.HSBtoRGB(index / colorCount.toFloat, saturation, brightness)
  }

  def RGBTest: Int => Int = { index =>
    val k = index.toDouble / (colorCount - 1)
    def shiftFromDistance(pos: Double): Int = {
      val dist = Math.abs(pos - k)
      if (dist > 1.0 / 8) 0
      else 255 - (dist * 8 * 255).toInt
    }
    if (k <= 1.0 / 8) {
      val s = shiftFromDistance(0)
      s << 16 | s << 8 | s
    } else if ( k >= 7.0 / 8) {
      val s = shiftFromDistance(1)
      s << 16 | s << 8 | s
    }
    else {
      val r = shiftFromDistance(1.0 / 4)
      val g = shiftFromDistance(1.0 / 2)
      val b = shiftFromDistance(3.0 / 4)
      r << 16 | g << 8 | b
    }
  }

  // dark blue: 237, 100, 35
  // dark yellow: 40, 100, 100
  def fromColors(colors: (Int, Float, Float)*): Int => Int = {
    require(colors.size > 1)
    val transitionRange = 1.0f / colors.size
    println(s"transRange: $transitionRange")
    def get_S_or_B(from: Float, to: Float, pos: Float): Float = {
      val dist = to - from
      from + dist * pos
    }
    def getHue(from: Int, to: Int, pos: Float): Int = {
      val dist = to - from
      if (Math.abs(dist) > 180) {
        if (to > from) getHue(from + 360, to, pos)
        else getHue(from, to + 360, pos)
      }
      else {
        (from + dist * pos).toInt % 360
      }
    }
    def f(index: Int): Int = {
      val k = index.toFloat / (colorCount)
      val i = (k * colors.size).toInt
      val (fromH, fromS, fromB) = colors(i)
      val (toH, toS, toB) = if (i + 1 == colors.size) colors(0) else colors(i+1)
      val pos = (k % transitionRange) * colors.size
      val h = getHue(fromH, toH, pos)
      val s = get_S_or_B(fromS, toS, pos)
      val b = get_S_or_B(fromB, toB, pos)
      println(s"index: $index, k: $k, i: $i, pos: $pos. ($h, $s, $b)")
      Color.HSBtoRGB(h / 360f, s, b)
    }
    f
  }

  val c2 = fromColors((237, 1f, 0.35f), (40, 1f, 1f))
  val c3 = fromColors((100, 0.5f, 0.5f), (200, 0.7f, 0.7f), (300, 0.5f, 0.5f))

  val blue_yellow = fromColors((236, 1f, 0.36f), (202, 0.44f, 0.95f), (160, 0.06f, 1f), (70, 0.12f, 0.98f), (40, 0.98f, 1f), (346, 0.85f, 0.36f))

  updateColors(blue_yellow)

  def top = new MainFrame {
    title = "ColorMapSb"
    contents = ui
  }

  /** Util method */
  def print(arr: Array[Int], msg: String = "Array"): Unit = {
    println(s"$msg: size: ${arr.size}")
    arr.foreach(println(_))
    println("-------------------------------")
  }

  /** Util method */
  def p(n: Int): String = {
    f"${Integer.toBinaryString(n)}%32s" replace(' ', '0')
  }
}
