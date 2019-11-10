package by.bsuir.dip

import java.nio.file.{Files, Paths}

import com.sksamuel.scrimage.nio.ImageWriter
import com.sksamuel.scrimage.{Color, Image, Pixel}

import scala.util.Random

object NoiseFilter {

  def addNoise(image: Image, noisePercent: Double, imageHeight: Int = 6, imageWidth: Int = 6): Image = {
    val nNoisePixels = (noisePercent * imageHeight * imageWidth).toInt

    val shuffledPixels = Random.shuffle(image.pixels.zipWithIndex.toList)
    val changedPixels = shuffledPixels.take(nNoisePixels).map {
      case (p, pos) if p.toColor == Color.White => (Pixel(Color.Black), pos)
      case (p, pos) if p.toColor == Color.Black => (Pixel(Color.White), pos)
    }
    val noisedImage =
      (changedPixels ++ shuffledPixels.drop(nNoisePixels))
        .sortBy(_._2)
        .map(_._1)
        .toArray
    Image(imageWidth, imageHeight, noisedImage)
  }

  def createSample(images: Seq[(String, Image)] = Images.ImagesWithNames,
                   noisePercents: Seq[Double] = Seq(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1d),
                   imageHeight: Int = 6,
                   imageWidth: Int = 6,
                   n: Int = 1) = {
    noisePercents.map { noise =>
      noise -> images.flatMap {
        case (name, image) =>
          for (i <- 0 until n) yield {
            val noiseImage = addNoise(image, noise, imageHeight, imageWidth)
            val imageFolder = s"images/$name/"
            val imageName = s"${name}_${noise}_$i.png"
            Files.createDirectories(Paths.get(imageFolder))
            noiseImage.forWriter(ImageWriter.default).write(imageFolder + imageName)
            (imageName, noiseImage)
          }
      }
    }
  }
}
