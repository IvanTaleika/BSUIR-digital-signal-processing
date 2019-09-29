package by.bsuir.dmi

import by.bsuir.dmi.ImageProcessor._
import com.sksamuel.scrimage.Image
import com.sksamuel.scrimage.nio.ImageWriter
import ObjectAnalyzer._
object Runner extends App {
  // TODO: сделать эрозию чтобы убрать все маленькие пиксели
  val image = Image.fromResource("/P0001460.jpg")
  val binImage = ImageProcessor.process(image)
  binImage.forWriter(ImageWriter.default).write("test.png")
  val objects = findObjects(binImage)
  val (areas, perimeters, massCenters, elongations) = new ImageAttributes(objects).attributes
  println("horay")

}
