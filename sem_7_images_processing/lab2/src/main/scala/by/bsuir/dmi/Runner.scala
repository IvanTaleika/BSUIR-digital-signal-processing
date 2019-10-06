package by.bsuir.dmi

import by.bsuir.dmi.ObjectAnalyzer._
import com.sksamuel.scrimage.nio.ImageWriter
import com.sksamuel.scrimage.{Image, Pixel}
object Runner extends App {
  val image = Image.fromResource("/P0001460.jpg")
  val binImage = ImageProcessor.process(image)
  val objects = findObjects(binImage)
  val attributes = new ImageAttributes(objects).attributes
  println("Attributes:")
  println(attributes.mkString("\n"))
  val model = KMedoidsModel(2, attributes.values.toSeq)
  println("Centers:")
  println(model.centers.mkString("\n"))
  val classifiedPixels: Map[(Int, Int), Int] = objects
    .map {
      case (i, s) =>
        val clusterIndex = model.classify(attributes(i))
        s.map(_ -> clusterIndex).toMap
    }
    .reduce(_ ++ _)
  val classifiedImage = binImage.map {    case (x, y, p) =>
      classifiedPixels.get((x, y)) match {
        case Some(0) => Pixel(255, 0, 0, 255)
        case Some(1) => Pixel(0, 255, 0, 255)
        case _       => p
      }
  }
  classifiedImage.forWriter(ImageWriter.default).write("result.png")

}
