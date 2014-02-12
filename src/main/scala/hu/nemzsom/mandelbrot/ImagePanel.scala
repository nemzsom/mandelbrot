package hu.nemzsom.mandelbrot

import scala.swing.Swing._
import scala.swing.Panel
import scala.swing.event.UIElementResized
import scala.swing.event.MousePressed
import scala.swing.event.MouseWheelMoved
import scala.swing.event.MouseDragged
import java.awt.{Dimension, Graphics2D}
import java.awt.image.BufferedImage
import rx.lang.scala.{Subscription, Observer, Observable}
import scala.swing.Reactions.Reaction

class ImagePanel(initialWidth: Int, initialHeight: Int) extends Panel {

  preferredSize = (initialWidth, initialHeight)
  focusable = true
  listenTo(this, mouse.moves, mouse.clicks, mouse.wheel)

  var image = new BufferedImage(initialWidth, initialHeight, BufferedImage.TYPE_INT_RGB)

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
          observer.onNext(diffX, diffY)
      }
    )
  }

  def reactionToObservable[T](r: Observer[T] => Reaction): Observable[T] = Observable.create(
    (observer: Observer[T]) => {
      val reaction = r(observer)
      reactions += reaction
      Subscription { reactions -= reaction}
    }
  )

  override def paintComponent(g: Graphics2D) = {
    super.paintComponent(g)
    g.drawImage(image, 0, 0, image.getWidth, image.getHeight, null)
  }

}
