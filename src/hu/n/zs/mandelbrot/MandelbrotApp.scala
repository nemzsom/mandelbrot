package hu.n.zs.mandelbrot

import scala.swing.Swing._
import scala.swing._
import scala.swing.event._
import java.awt.{Color, Graphics2D, Point}
import java.awt.image.BufferedImage
import java.awt.image.BufferedImageOp

object MandelbrotApp extends SimpleSwingApplication {
  
  lazy val ui = new Panel {
    background = Color.white
    preferredSize = (640,480)
    val lineRGB = Color.black.getRGB
    var bufferedImage: BufferedImage = new BufferedImage(640, 480, BufferedImage.TYPE_INT_ARGB)
    var actPoint: Point = new Point()

    focusable = true
    listenTo(mouse.clicks, mouse.moves, keys, this)
    
    reactions += {
      case e: MousePressed => 
        moveTo(e.point)
        requestFocusInWindow()
      case e: MouseDragged  => lineTo(e.point)
      case e: MouseReleased => lineTo(e.point)
      case KeyTyped(_,'c',_,_) => 
        bufferedImage = new BufferedImage(size.width, size.height, BufferedImage.TYPE_INT_ARGB) 
        repaint()
      case e: UIElementResized => println(s"resized e: $e")
      case _: FocusLost => repaint()
    }


    def lineTo(p: Point) {
      println(s"lineTo: $p")
      val graphics = bufferedImage.createGraphics()
      graphics.setColor(Color.black)
      graphics.drawLine(actPoint.x, actPoint.y, p.x, p.y)
      actPoint = p
      repaint() 
    }
    
    def moveTo(p: Point) {
      println(s"moveTo: $p")
      actPoint = p
      //bufferedImage.setRGB(p.x, p.y, lineRGB)
      //repaint()
    }

    override def paintComponent(g: Graphics2D) = {
      super.paintComponent(g)
      g.drawImage(bufferedImage, 0, 0, 640, 480, null)
    }
  }
  
  def top = new MainFrame {
    title = "Mandelbrot set"
    contents = ui
  }

}