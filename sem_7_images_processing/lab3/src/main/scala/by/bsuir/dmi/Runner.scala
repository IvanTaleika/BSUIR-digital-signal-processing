package by.bsuir.dmi

import java.nio.file.Paths

import com.sksamuel.scrimage.nio.ImageWriter
import com.sksamuel.scrimage.{Color, Image, Pixel}

import scala.util.Random

object Runner extends App {

  val NNoiseImages = 10

  def rgbSum(p: Pixel) = p.red + p.green + p.blue
  Seq("letter_e", "letter_o", "letter_sh").par.foreach { im =>
    val letter = Image.fromPath(Paths.get(s"images/$im/$im.png"))
    Seq(10, 20, 30, 35, 40, 45, 50, 60, 70, 80, 90, 100).foreach { percent =>
      for (i <- 0 until NNoiseImages) {
        val shuffledLetterPixels = Random.shuffle(letter.pixels.zipWithIndex.toList)
        val changedPixels = shuffledLetterPixels.take(percent).map {
          case (p, pos) if p.toColor == Color.White => (Pixel(Color.Black), pos)
          case (p, pos) if p.toColor == Color.Black => (Pixel(Color.White), pos)
        }
        val noiseLetterPixels =
          (changedPixels ++ shuffledLetterPixels.drop(percent)).sortBy(_._2).map(_._1).toArray
        val noiseLetter = Image(10, 10, noiseLetterPixels)

        noiseLetter.forWriter(ImageWriter.default).write(s"images/$im/$percent/${im}_$i.png")
      }
    }

  }
}
