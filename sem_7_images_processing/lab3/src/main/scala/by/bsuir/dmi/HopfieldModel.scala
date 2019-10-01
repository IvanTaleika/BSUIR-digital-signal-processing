package by.bsuir.dmi

import breeze.linalg.{DenseMatrix, DenseVector}
import com.sksamuel.scrimage.{Color, Image, Pixel}

class HopfieldModel(val letterImages: Seq[Image]) {
  val coeffs: DenseMatrix[Int] = letterImages
    .map(toWeightsVector)
    .map(v => v * v.t)
    .reduceLeft(_ + _)

  def recognize(im: Image): Image = {
    var weights = toWeightsVector(im)
    var recognizedWeights = recognize(weights)
    while (recognizedWeights != weights) {
      weights = recognizedWeights
      recognizedWeights = recognize(weights)
    }
    val pixels = recognizedWeights.map(w => if (w == 1) Pixel(Color.Black) else Pixel(Color.White)).toArray
    Image(10, 10, pixels)
  }

  def recognize(ws: DenseVector[Int]): DenseVector[Int] = {
    (ws.t * coeffs).t.map(w => if (w < 0) -1 else 1)
  }

  private def toWeightsVector(im: Image) = {
    DenseVector[Int](
      im.pixels
        .map {
          case p if p.toColor == Color.White => -1
          case p if p.toColor == Color.Black => 1
        })

  }
}
