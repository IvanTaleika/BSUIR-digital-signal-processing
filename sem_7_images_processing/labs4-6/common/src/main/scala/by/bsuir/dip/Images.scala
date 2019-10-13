package by.bsuir.dip

import breeze.linalg.DenseVector
import com.sksamuel.scrimage.{Color, Image}

object Images {
  val Names = Seq("k" , "l", "o", "t", "u")
  val ImagesWithNames = Names.map(n => n -> Image.fromResource(s"/$n.png"))

   def toWeightsVector(im: Image) = {
    DenseVector[Double](
      im.pixels
        .map {
          case p if p.toColor == Color.White => 0d
          case p if p.toColor == Color.Black => 1d
        })

  }
}
