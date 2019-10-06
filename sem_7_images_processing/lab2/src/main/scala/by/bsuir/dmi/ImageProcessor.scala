package by.bsuir.dmi

import com.sksamuel.scrimage.{Color, Image, Pixel}

import scala.util.Try

object ImageProcessor {

  def process(i: Image): Image = dilation(erosion(binarize(median(grayscale(i)))))

  implicit object pixelOrdering extends Ordering[Pixel] {
    override def compare(x: Pixel, y: Pixel): Int = rgbSum(x) - rgbSum(y)
  }

  def rgbSum(p: Pixel) = p.red + p.green + p.blue

  def windowFunction(i: Image, windowFunction: Seq[Pixel] => Pixel, forPixel: Pixel => Boolean = _ => true) = {
    i.map {
      case (x, y, p) if forPixel(p) => windowFunction(window(x, y, i))
      case (_, _, p)                => p
    }
  }

  def erosion(i: Image) = windowFunction(i, _.min, _.toColor == Color.White)
  def dilation(i: Image) = windowFunction(i, _.max, _.toColor == Color.Black)
  def median(i: Image) = windowFunction(i, w => w.sorted.apply(5))

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

  def grayscale(i: Image) = {
    i.map { (_, _, p) =>
      val gray = (0.3 * p.red).toInt + (0.59 * p.green).toInt + (0.11 * p.blue).toInt
      Pixel(gray, gray, gray, 255)
    }
  }

}
