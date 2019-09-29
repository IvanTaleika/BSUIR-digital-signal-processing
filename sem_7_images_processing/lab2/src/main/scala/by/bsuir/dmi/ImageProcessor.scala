package by.bsuir.dmi

import com.sksamuel.scrimage.{Color, Image, Pixel}

import scala.util.Try

object ImageProcessor {

  def process(i: Image): Image = dilation(erosion(binarize(i)))

  implicit object pixelOrdering extends Ordering[Pixel] {
    override def compare(x: Pixel, y: Pixel): Int = rgbSum(x) - rgbSum(y)
  }

  def rgbSum(p: Pixel) = p.red + p.green + p.blue

  def windowFunction(i: Image, windowFunction: Seq[Pixel] => Pixel, forColor: Color) = {
    i.map {
      case (x, y, p) if p.toColor == forColor => windowFunction(window(x, y, i))
      case (_, _, p)                          => p
    }
  }

  def erosion(i: Image) = windowFunction(i, _.min, Color.White)
  def dilation(i: Image) = windowFunction(i, _.max, Color.Black)

  def window(x: Int, y: Int, i: Image): Seq[Pixel] = {
    def prev(n: Int) = n - 1
    def next(n: Int) = n + 1
    for (x <- prev(x) to next(x); y <- prev(y) to next(y)) yield Try(i.pixel(x, y)).getOrElse(Pixel(Color.Black))
  }

  def binarize(i: Image) = i.map { (_, _, p) =>
    if (rgbSum(p) / 3 > 220) {
      Pixel(Color.White)
    } else {
      Pixel(Color.Black)
    }
  }

}
