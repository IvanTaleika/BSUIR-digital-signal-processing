package by.bsuir.dmi

import com.sksamuel.scrimage.{Color, Image}

import scala.collection.mutable

object ObjectAnalyzer {
  def findObjects(binImage: Image): Map[Int, Seq[(Int, Int)]] = {
    var nObjects = 0
    val pixelClusters = mutable.HashMap.empty[(Int, Int), Int]

    binImage.foreach { (x, y, p) =>
      if (p.toColor == Color.White) {
        val curr = (x, y)
        val left = (x - 1, y)
        val top = (x, y - 1)

        (pixelClusters.get(left), pixelClusters.get(top)) match {
          case (Some(li), Some(ti)) =>
            pixelClusters ++= pixelClusters.filter(t => t._2 == ti).mapValues(_ => li)
            pixelClusters.put(curr, li)
          case (Some(li), None) => pixelClusters.put(curr, li)
          case (None, Some(ti)) => pixelClusters.put(curr, ti)
          case _ =>
            nObjects += 1
            pixelClusters.put(curr, nObjects)
        }
      }
    }
    pixelClusters.groupBy(_._2).map(_._2.keys.toSeq).zipWithIndex.map(_.swap).toMap
  }
}
