package by.bsuir.dmi

import scala.collection.immutable.{IntMap, Map}
import scala.collection.parallel.ParMap
import scala.math.{pow, sqrt}
class ImageAttributes(objects: Map[Int, Seq[(Int, Int)]]) {
  def toIntMap[T](m: ParMap[Int, T]): IntMap[T] = toIntMap(m.seq.toMap)
  def toIntMap[T](m: Map[Int, T]): IntMap[T] = IntMap(m.toSeq: _*)

  val normalizedObject = objects.mapValues { obj =>
    val minX = obj.map(_._1).min
    val minY = obj.map(_._2).min
    obj.map { case (x, y) => (x - minX, y - minY) }.par
  }.par

  val areas = normalizedObject.mapValues(_.size.toDouble)

  val perimeters = normalizedObject
    .mapValues { obj =>
      obj.count {
        case (x, y) => !Seq((x - 1, y), (x, y - 1), (x + 1, y), (x, y + 1)).forall(p => obj.exists(_ == p))
      }.toDouble
    }

  val massCenters = normalizedObject
    .mapValues { obj =>
      val nPixels = obj.size
      val (xSum, ySum) = obj.foldLeft((0, 0)) {
        case ((x1, y1), (x2, y2)) => (x1 + x2, y1 + y2)
      }
      (xSum / nPixels, ySum / nPixels)
    }


  val massCenterModules = massCenters.mapValues { case (x, y) => sqrt(x * x + y * y) }

  val elongations = normalizedObject
    .map {
      case (k, v) => (k, (v, massCenters(k)))
    }
    .mapValues {
      case (obj, (xMass, yMass)) =>
        val m20 = obj.map { case (x, _) => pow(x - xMass, 2) }.sum
        val m02 = obj.map { case (_, y) => pow(y - yMass, 2) }.sum
        val m11 = obj.map { case (x, y) => (x - xMass) * (y - yMass) }.sum
        val left = m20 + m02
        val right = sqrt(pow(m20 - m02, 2) + 4 * pow(m11, 2))
        (left + right) / (left - right)
    }


  def normalizeAttribute(attributes: ParMap[Int, Double]) = {
    val maxVal = attributes.values.max
    attributes.mapValues(_ / maxVal)
  }

  val attributes: Map[Int, Attributes] = Seq(areas, perimeters, massCenterModules, elongations)
    .map(a => toIntMap(normalizeAttribute(a)))
    .foldLeft(toIntMap(objects.mapValues(_ => Seq.empty[Double])))((m1, m2) =>
      m1.intersectionWith(m2, (_, v1: Seq[Double], v2: Double) => v1 :+ v2))
    .mapValues {
      case Seq(a, p, mc, e) => Attributes(a, p, mc, e)
    }

}
