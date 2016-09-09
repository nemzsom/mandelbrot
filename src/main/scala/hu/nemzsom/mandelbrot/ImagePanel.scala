package hu.nemzsom.mandelbrot

import scala.swing.Swing._
import scala.swing.Panel
import scala.swing.event._
import java.awt.{Dimension, Graphics2D}
import java.awt.image.BufferedImage
import rx.lang.scala.{Subscription, Observer, Observable}
import scala.swing.Reactions.Reaction
import scala.swing.event.MousePressed
import scala.swing.event.UIElementResized
import scala.swing.event.MouseWheelMoved
import scala.swing.event.MouseDragged

class ImagePanel(initialWidth: Int, initialHeight: Int) extends Panel {

  private var _width = initialWidth
  private var _height = initialHeight
  def width = _width
  def height = _height

  private var _image = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
  def image = _image

  preferredSize = (initialWidth, initialHeight)
  focusable = true
  listenTo(this, mouse.moves, mouse.clicks, mouse.wheel, keys)

  val mouseWheelMoved: Observable[MouseWheelMoved] = reactionToObservable (
    (observer: Observer[MouseWheelMoved]) => {
      case e: MouseWheelMoved => observer.onNext(e)
    }
  )

  val resized : Observable[Dimension] = reactionToObservable (
    (observer: Observer[Dimension]) => {
      case _: UIElementResized => observer.onNext(size)
    }
  )

  val mouseDragged : Observable[(Int, Int)] = {
    var draggedFrom: java.awt.Point = (0, 0)
    reactionToObservable (
      (observer: Observer[(Int, Int)]) => {
        case e: MousePressed => draggedFrom = e.point
        case e: MouseDragged =>
          val diffX = e.point.x - draggedFrom.x
          val diffY = e.point.y - draggedFrom.y
          draggedFrom = e.point
          observer.onNext((diffX, diffY))
      }
    )
  }

  val keyPressed: Observable[KeyPressed] = reactionToObservable{
    (observer: Observer[KeyPressed]) => {
      case kp: KeyPressed => observer.onNext(kp)
    }
  }

  def reactionToObservable[T](r: Observer[T] => Reaction): Observable[T] = Observable.create(
    (observer: Observer[T]) => {
      val reaction = r(observer)
      reactions += reaction
      Subscription { reactions -= reaction}
    }
  )

  def resizeImage(newWidth: Int, newHeight: Int): Unit = {
    _width = newWidth
    _height = newHeight
    val newImage = new BufferedImage(_width, _height, BufferedImage.TYPE_INT_RGB)
    val g = newImage.getGraphics
    g.drawImage(_image, 0, 0, null)
    g.dispose()
    _image = newImage
  }

  def moveImage(diffX: Int, diffY: Int): Unit = {
    val newImage = new BufferedImage(_width, _height, BufferedImage.TYPE_INT_RGB)
    val g = newImage.getGraphics
    g.drawImage(_image, diffX, diffY, null)
    g.dispose()
    _image = newImage
  }

  def zoomImage(factor: Double, at: (Int, Int)): Unit = {
    val (atX, atY) = at
    val newImage = new BufferedImage(_width, _height, BufferedImage.TYPE_INT_RGB)
    val g = newImage.getGraphics
    g.drawImage(scaleImage(_image, factor), (atX - atX * factor).toInt, (atY - atY * factor).toInt, null)
    g.dispose()
    _image = newImage
  }

  def scaleImage(img: BufferedImage, factor: Double): BufferedImage = {
    val width = (img.getWidth * factor).toInt
    val height = (img.getHeight * factor).toInt
    val scaled = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB)
    val g = scaled.createGraphics
    // TODO find rendering hints for fast scale
    //g.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BILINEAR)
    //g.setRenderingHint(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY)
    //g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
    g.drawImage(img, 0, 0, width, height, null)
    scaled
  }

  override def paintComponent(g: Graphics2D) = {
    super.paintComponent(g)
    g.drawImage(_image, 0, 0, _image.getWidth, _image.getHeight, null)
  }

}
