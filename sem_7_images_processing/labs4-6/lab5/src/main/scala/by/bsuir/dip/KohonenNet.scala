package by.bsuir.dip

import by.bsuir.dip
import com.sksamuel.scrimage.Image

import scala.collection.immutable
import scala.math._
import scala.util.Random

class KohonenNet(clusters: Seq[Cluster]) {

  def fit(image: Image): (String, Image) = {
    val pixels = Images.toWeightsArray(image)
    clusters
      .maxBy(n => n.coordinates.zip(pixels).foldLeft(0d) { case (sum, (w, x)) => sum + w * x })
      .image
  }
}

class KohonenNetModel(speed: Double = 0.3, step: Double = -0.01) {

  def train(data: Seq[(String, Image)]): KohonenNet = train(data, data.length)

  def train(data: Seq[(String, Image)], nClusters: Int): KohonenNet = {
//    val neurons = data.map{ case d @ (_, image) => Neuron(Images.toWeightsArray(image), d)}
//    new KohonenNet(neurons)

    val inputs = data.map { case (_, image) => Images.toWeightsArray(image) }
    val nLinks = inputs.head.length
    var neurons =
      (0 until nClusters).map { _ =>
        val weights = for (_ <- 1 to nLinks) yield (1 - Random.nextDouble() * 2) / sqrt(nClusters)
        Neuron(weights, 0)
      }

    BigDecimal(speed).until(0).by(step).foreach { s =>
      for (_ <- 1 to 1000) {
        inputs.foreach { pixels =>
          val i = minDistance(neurons, pixels)
          val closestNeuron = neurons(i)
          val updatedNeuron = Neuron(weightUpdate(closestNeuron.weights, pixels, s.toDouble), closestNeuron.wins + 1)
          neurons = neurons.updated(i, updatedNeuron)
        }
      }
    }
    val clusters = for (i <- data.indices) yield {
      val coordinates = neurons(minDistance(neurons, inputs(i))).weights
      Cluster(coordinates, data(i))
    }
    new KohonenNet(clusters)
  }

  def vectorsDistance(xs: Seq[Double], neuron: Neuron): Double =
    xs.zip(neuron.weights).map { case (x, w) => abs(x - w) * neuron.wins }.sum

  def minDistance(neurons: Seq[Neuron], input: Seq[Double]): Int = {
    neurons
      .map(n => vectorsDistance(input, n))
      .zipWithIndex
      .minBy(_._1)
      ._2
  }

  def weightUpdate(ws: Seq[Double], xs: Seq[Double], speed: Double): immutable.IndexedSeq[Double] =
    ws.indices.map(i => ws(i) + speed * (xs(i) - ws(i)))
}

case class Neuron(weights: Seq[Double], wins: Int)
case class Cluster(coordinates: Seq[Double], image: (String, Image))
