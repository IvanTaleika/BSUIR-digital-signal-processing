package by.bsuir.dip

import breeze.linalg.DenseVector
import com.sksamuel.scrimage.{Color, Image}

object Images {
  val Names = Seq("K", "L", "O", "T", "U")
  val ImagesWithNames = Names.map(n => n -> Image.fromResource(s"/$n.png"))

  def toWeightsVector(im: Image) = {
    DenseVector[Double](toWeightsArray(im))
  }

  def toWeightsArray(im: Image): Array[Double] = {
    im.pixels
      .map {
        case p if p.toColor == Color.White => -1d
        case p if p.toColor == Color.Black => 1d
      }
  }
}
