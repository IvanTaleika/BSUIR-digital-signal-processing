package by.bsuir.dmi

import scala.util.Random

class KMedoidsModel(val centers: Seq[Attributes]) {
  def classify(a: Attributes): Int = centers.zipWithIndex.minBy(_._1 - a)._2
}

object KMedoidsModel {
  def apply(k: Int, data: Seq[Attributes], iterations: Int = 8, maxIterations: Int = 24): KMedoidsModel = {
    val shuffledData = Random.shuffle(data)
    val firstMedoids = shuffledData.take(k)
    val firstData = shuffledData.drop(k)
    val firstDistance =
      firstData.map(a => firstMedoids.map(_ - a).zipWithIndex.minBy(_._1)).foldLeft(0d)(_ + _._1.module)

    def findMedoids(medoids: Seq[Attributes],
                    data: Seq[Attributes],
                    distance: Double,
                    iterationsLeft: Int,
                    maxIterationsLeft: Int): Seq[Attributes] = {
      if (iterationsLeft == 0 || maxIterationsLeft == 0) {
        medoids
      } else {
        val newMedoids = medoids.tail :+ Random.shuffle(data).head
        val newData = data :+ medoids.head
        val newDistance =
          newData.map(a => medoids.map(_ - a).zipWithIndex.minBy(_._1)).foldLeft(0d)(_ + _._1.module)
        if (newDistance >= distance) {
          findMedoids(medoids, data, distance, iterationsLeft - 1, maxIterationsLeft - 1)
        } else {
          findMedoids(newMedoids, newData, newDistance, iterations, maxIterationsLeft - 1)
        }
      }
    }

    val medoids = findMedoids(firstMedoids, firstData, firstDistance, iterations - 1, maxIterations)
    new KMedoidsModel(medoids)
  }

}
