package hu.nemzsom.sandbox

import scala.swing.Swing._
import scala.swing.{ MainFrame, Panel }
import scala.swing.event._
import java.awt._
import java.awt.image.BufferedImage
import scala.swing.SimpleSwingApplication

object LineDrawExample extends SimpleSwingApplication {

  lazy val ui = new Panel {
    background = Color.white
    preferredSize = (640, 480)
    val lineRGB = Color.black.getRGB
    var bufferedImage: BufferedImage = new BufferedImage(640, 480, BufferedImage.TYPE_INT_ARGB)
    var actPoint = new Point()

    focusable = true
    listenTo(mouse.clicks, mouse.moves, keys, this)

    reactions += {
      case e: MousePressed =>
        moveTo(e.point)
        requestFocusInWindow()
      case e: MouseDragged => lineTo(e.point)
      case e: MouseReleased => lineTo(e.point)
      case KeyTyped(_, 'c', _, _) => clear()
      case e: UIElementResized => resize()
      case _: FocusLost => repaint()
    }

    def lineTo(p: java.awt.Point): Unit = {
      val graphics = bufferedImage.createGraphics()
      graphics.setColor(Color.black)
      graphics.drawLine(actPoint.x, actPoint.y, p.x, p.y)
      actPoint = p
      repaint()
    }

    def moveTo(p: java.awt.Point): Unit = {
      actPoint = p
    }

    def clear(): Unit = {
      bufferedImage = new BufferedImage(size.width, size.height, BufferedImage.TYPE_INT_ARGB)
      repaint()
    }

    def resize(): Unit = {
      val newImg = new BufferedImage(size.width, size.height, BufferedImage.TYPE_INT_ARGB)
      val graphics = newImg.createGraphics
      graphics.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BILINEAR)
      graphics.setRenderingHint(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY)
      graphics.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
      graphics.drawImage(bufferedImage, 0, 0, size.width, size.height, null)
      bufferedImage = newImg
      repaint()
    }

    override def paintComponent(g: Graphics2D) = {
      super.paintComponent(g)
      g.drawImage(bufferedImage, 0, 0, bufferedImage.getWidth, bufferedImage.getHeight, null)
    }
  }

  def top = new MainFrame {
    title = "Line draw"
    contents = ui
  }
}