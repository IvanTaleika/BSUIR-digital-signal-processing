package by.bsuir.dip

import breeze.linalg.{DenseVector, sum}
import breeze.numerics._
import com.sksamuel.scrimage.Image

import scala.annotation.tailrec
import scala.collection.immutable
import scala.reflect.ClassTag
import scala.util.Random

class MultilayerNet(hideLayer: Seq[Neuron],
                    outLayer: Seq[Neuron],
                    clusters: Map[(String, Image), DenseVector[Double]]) {
  import MultilayerNet._

  def fit(image: Image): (String, Image) = {
    val input = Images.toWeightsVector(image)
    clusters.mapValues(output => meanSquaredError(invoke(input, output, hideLayer, outLayer)._3)).minBy(_._2)._1
  }
}

object MultilayerNet {
  def meanSquaredError(neuronErrors: DenseVector[Double]): Double =
    neuronErrors.reduce((e1, e2) => e1 * e1 + e2 * e2) / 2

  type InvocationOutputs = (DenseVector[Double], DenseVector[Double], DenseVector[Double])

  def invoke(input: DenseVector[Double],
             output: DenseVector[Double],
             hideLayer: Seq[Neuron],
             outLayer: Seq[Neuron]): InvocationOutputs = {
    val hideLayerValues = toDV(hideLayer.map(neuronValue(_, input)))
    val outLayerValues = toDV(outLayer.map(neuronValue(_, hideLayerValues)))
    val errors = output - outLayerValues
    (hideLayerValues, outLayerValues, errors)
  }

  private def neuronValue(p: Neuron, input: DenseVector[Double]) =
    ActivationFunctions.sigmoid(input.t * p.weights + p.threshold)

  def toDV[T: ClassTag](s: Seq[T]): DenseVector[T] = DenseVector(s: _*)

}

class MultilayerNetModel(maxError: Double = 0.05, speedA: Double = 0.01, speedB: Double = 0.01) {

  import MultilayerNet._

  private def isTrained(errors: Seq[DenseVector[Double]]): Boolean = errors.map(meanSquaredError).max < maxError

  def train(data: Seq[(String, Image)]) = {
    val inSize = data.head._2.pixels.length
    val outSize = data.length
    val hideSize = (2d * (inSize + outSize) / 3d).toInt
    val hideLayer: immutable.Seq[Neuron] = (0 until hideSize).map(_ => randomNeuron(inSize))
    val outLayer = (0 until outSize).map(_ => randomNeuron(hideSize))
    val inputs = data.map { case (_, image)             => Images.toWeightsVector(image) }
    val outputs = data.zipWithIndex.map { case (_, idx) => toDV(Seq.fill(outSize)(0d).updated(idx, 1d)) }
    val trainOutputs = invokeAll(hideLayer, outLayer, inputs, outputs)
    val (resultHideLayer, resultOutLayer) = adjustNeurons(trainOutputs, hideLayer, outLayer, inputs, outputs)
    new MultilayerNet(resultHideLayer, resultOutLayer, data.zip(outputs).toMap)

  }

  private def invokeAll(hideLayer: Seq[Neuron],
                        outLayer: Seq[Neuron],
                        inputs: Seq[DenseVector[Double]],
                        outputs: Seq[DenseVector[Double]]) =
    inputs.zip(outputs).foldRight(List.empty[InvocationOutputs]) {
      case ((input, output), trainOutput) => invoke(input, output, hideLayer, outLayer) :: trainOutput
    }

  @tailrec
  private def adjustNeurons(trainOutputs: Seq[InvocationOutputs],
                   hideLayer: Seq[Neuron],
                   outLayer: Seq[Neuron],
                   inputs: Seq[DenseVector[Double]],
                   outputs: Seq[DenseVector[Double]]): (Seq[Neuron], Seq[Neuron]) = {
    if (isTrained(trainOutputs.map(_._3))) {
      (hideLayer, outLayer)
    } else {
      val (nextHideLayer, nextOutLayer) = trainOutputs.zipWithIndex.foldLeft((hideLayer, outLayer)) {
        case ((hideLayer, outLayer),
              ((gs: DenseVector[Double], ys: DenseVector[Double], es: DenseVector[Double]), i)) =>
          val nextOutLayer = outLayer.zipWithIndex.map { case (n, k) => adjustOutNeuron(n, gs, ys(k), es(k)) }
          val nextHideLayer = hideLayer.zipWithIndex.map {
            case (n, j) =>
              adjustHideNeuron(n, gs(j), ys, es, toDV(outLayer.map(_.weights(j))), inputs(i))
          }
          (nextHideLayer, nextOutLayer)
      }
      val nextTrainOutputs = invokeAll(hideLayer, outLayer, inputs, outputs)
      adjustNeurons(nextTrainOutputs, nextHideLayer, nextOutLayer, inputs, outputs)
    }
  }

  private def adjustOutNeuron(n: Neuron, gs: DenseVector[Double], yk: Double, ek: Double) = {
    val coeff = speedA * yk * (1 - yk) * ek
    val ws = toDV(n.weights.activeIterator.map {
      case (j, w) =>
        w + coeff * gs(j)
    }.toSeq)
    val t = n.threshold + coeff
    Neuron(ws, t)
  }

  private def adjustHideNeuron(n: Neuron,
                               gj: Double,
                               ys: DenseVector[Double],
                               es: DenseVector[Double],
                               wjks: DenseVector[Double],
                               inputI: DenseVector[Double]) = {
    val ej = sum(wjks * es * ys.map(y => y * (1 - y)))
    val coeff = speedB * gj * (1 - gj) * ej
    val vs = toDV(n.weights.activeIterator.map { case (i, v) => v + coeff * inputI(i) }.toSeq)
    val t = n.threshold + coeff
    Neuron(vs, t)
  }

  private def randomNeuron(weightSize: Int) = Neuron(randomWeights(weightSize), Random.nextDouble())

  private def randomWeights(s: Int): DenseVector[Double] = DenseVector.rand[Double](s)
}

case class Neuron(weights: DenseVector[Double], threshold: Double)

object ActivationFunctions {
  val sigmoid: Double => Double = { n: Double =>
    1d / (1d + exp(-n))
  }

  val bipolarSigmoid: Double => Double = { n: Double =>
    2d / (1d + exp(-n)) - 1
  }
}
