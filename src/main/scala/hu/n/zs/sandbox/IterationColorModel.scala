package hu.n.zs.sandbox

import java.awt.image.{Raster, DataBuffer, ColorModel}
import java.awt.color.ColorSpace
import java.awt.Transparency

class IterationColorModel(maxIter: Integer) extends ColorModel(32, Array[Int](8, 8, 8), ColorSpace.getInstance(ColorSpace.CS_sRGB),
                                                                      false, false, Transparency.OPAQUE, DataBuffer.TYPE_INT) {
  def getRed(pixel: Int): Int = 100

  def getGreen(pixel: Int): Int = 100

  def getBlue(pixel: Int): Int = 100//255 * pixel / maxIter

  def getAlpha(pixel: Int): Int = 1

  // FIXME: hack
  override def isCompatibleRaster(raster: Raster): Boolean = {
    true
  }
}
