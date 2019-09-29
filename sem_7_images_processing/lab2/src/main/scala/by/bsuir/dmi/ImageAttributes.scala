package by.bsuir.dmi

import by.bsuir.dmi.Runner.objects

import scala.math.{pow, sqrt}

class ImageAttributes(obj: Map[Int, Seq[(Int, Int)]]) {
  val areas = objects.mapValues(_.size)

  val perimeters = objects.mapValues { obj =>
    obj.count {
      case (x, y) => !Seq((x - 1, y), (x, y - 1), (x + 1, y), (x, y + 1)).forall(obj.contains)
    }
  }

  val normalizedObject = objects.mapValues { obj =>
    val minX = obj.map(_._1).min
    val minY = obj.map(_._2).min
    obj.map { case (x, y) => (x - minX, y - minY) }
  }
  val massCenters: Map[Int, (Int, Int)] = normalizedObject.mapValues { obj =>
    val nPixels = obj.size
    val (xSum, ySum) = obj.foldLeft((0, 0)) {
      case ((x1, y1), (x2, y2)) => (x1 + x2, y1 + y2)
    }
    (xSum / nPixels, ySum / nPixels)
  }

  val elongations: Map[Int, Double] = normalizedObject
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

  val attributes = (areas, perimeters, massCenters, elongations)
}
