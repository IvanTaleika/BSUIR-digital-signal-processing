package by.bsuir.dip

import breeze.linalg.{DenseVector, sum}
import breeze.numerics._
import com.sksamuel.scrimage.Image

import scala.collection.immutable
import scala.reflect.ClassTag
import scala.util.Random

class MultilayerNet(hideLayer: Seq[Perceptron],
                    outLayer: Seq[Perceptron],
                    clusters: Map[DenseVector[Double], (String, Image)]) {
  import MultilayerNet._

  def fit(image: Image): Map[Double, (String, Image)] = {
    val input = Images.toWeightsVector(image)
    clusters.map{case (output, desc) => meanSquaredError(invoke(input, output, hideLayer, outLayer)._3) -> desc}
  }
}

object MultilayerNet {
  def meanSquaredError(neuronErrors: DenseVector[Double]): Double =
    neuronErrors.reduce((e1, e2) => e1 * e1 + e2 * e2) / 2

  type InvocationOutputs = (DenseVector[Double], DenseVector[Double], DenseVector[Double])

  def invoke(input: DenseVector[Double],
             output: DenseVector[Double],
             hideLayer: Seq[Perceptron],
             outLayer: Seq[Perceptron]) = {
    val hideLayerValues = toDV(hideLayer.map(perceptronValue(_, input)))
    val outLayerValues = toDV(outLayer.map(perceptronValue(_, hideLayerValues)))
    val errors = outLayerValues - output
    (hideLayerValues, outLayerValues, errors)
  }

  private def perceptronValue(p: Perceptron, input: DenseVector[Double]) =
    ActivationFunctions.sigmoid(input.t * p.weights + p.threshold)

  def toDV[T: ClassTag](s: Seq[T]): DenseVector[T] = DenseVector(s: _*)

}

class MultilayerNetModel(maxError: Double = 0.05, speedA: Double = 0.01, speedB: Double = 0.5) {

  import MultilayerNet._

  private def isTrained(errors: Seq[DenseVector[Double]]): Boolean = errors.map(meanSquaredError).max < maxError

  def train(data: Seq[(String, Image)], n: Int = 100, hideLayerSize: Int = -1) = {
//    val hideSize = if (hideLayerSize == -1) sqrt(n / data.length).toInt else hideLayerSize
    val hideSize = 6
    val hideLayer: immutable.Seq[Perceptron] = (0 until hideSize).map(_ => randomPerceptron(data.head._2.pixels.length))
    val outSize = data.length
    val outLayer = (0 until outSize).map(_ => randomPerceptron(hideSize))
    val inputs = data.map {
      case (_, image) => Images.toWeightsVector(image)
    }
    val outputs = data.zipWithIndex.map {
      case (_, idx) => toDV(Seq.fill(outSize)(0d).updated(idx, 1d))
    }
    // TODO: do this 100 times?
    val trainOutputs = invokeAll(hideLayer, outLayer, inputs, outputs)
    val (resultHideLayer, resultOutLayer) = temp(trainOutputs, hideLayer, outLayer, inputs, outputs)
    new MultilayerNet(resultHideLayer, resultOutLayer, outputs.zip(data).toMap)

  }

  private def invokeAll(hideLayer: Seq[Perceptron],
                        outLayer: Seq[Perceptron],
                        inputs: Seq[DenseVector[Double]],
                        outputs: Seq[DenseVector[Double]]) =
    inputs.zip(outputs).foldLeft(List.empty[InvocationOutputs]) {
      case (trainOutput, (input, output)) => invoke(input, output, hideLayer, outLayer) :: trainOutput
    }

  private def temp(trainOutputs: Seq[InvocationOutputs],
                  hideLayer: Seq[Perceptron],
                  outLayer: Seq[Perceptron],
                  inputs: Seq[DenseVector[Double]],
                  outputs: Seq[DenseVector[Double]]): (Seq[Perceptron], Seq[Perceptron]) = {
    if (isTrained(trainOutputs.map(_._3))) {
      (hideLayer, outLayer)
    } else {
      val (nextHideLayer, nextOutLayer) = trainOutputs.zipWithIndex.foldLeft((hideLayer, outLayer)) {
        case ((hideLayer, outLayer),((gs: DenseVector[Double], ys: DenseVector[Double], es: DenseVector[Double]), i)) =>
          val nextOutLayer = outLayer.zipWithIndex.map {
            case (p, k) => adjustOutPerceptron(p, gs, ys(k), es(k))
          }
          val nextHideLayer = hideLayer.zipWithIndex.map {
            case (p, j) =>
              adjustHidePerceptron(p, gs(j), ys, es, toDV(outLayer.map(_.weights(j))), inputs(i))
          }
          (nextHideLayer, nextOutLayer)
      }
      val nexttrainOutputs = invokeAll(hideLayer, outLayer, inputs, outputs)
      temp(nexttrainOutputs, nextHideLayer, nextOutLayer, inputs, outputs)
    }
  }

  private def adjustOutPerceptron(p: Perceptron, gs: DenseVector[Double], yk: Double, ek: Double) = {
    val coeff = speedA * yk * (1 - yk) * ek
    val ws = toDV(p.weights.activeIterator.map { case (j, w) => w + coeff * gs(j) }.toSeq)
    val t = p.threshold + coeff
    Perceptron(ws, t)
  }

  private def adjustHidePerceptron(p: Perceptron,
                                   gj: Double,
                                   ys: DenseVector[Double],
                                   es: DenseVector[Double],
                                   wjs: DenseVector[Double],
                                   inputI: DenseVector[Double]) = {
    val ej = sum(wjs * es * ys.map(y => y * (1 - y)))
    val coeff = speedB * gj * (1 - gj) * ej
    val ws = toDV(p.weights.activeIterator.map { case (i, w) => w + coeff * inputI(i) }.toSeq)
    val t = p.threshold + coeff
    Perceptron(ws, t)
  }

  private def randomPerceptron(weightSize: Int) = Perceptron(randomWeights(weightSize), nextRandomCoeff())

  private def nextRandomCoeff(n: Double = Random.nextDouble()) = n * 2 - 1

  private def randomWeights(s: Int): DenseVector[Double] = DenseVector.rand[Double](s).map(nextRandomCoeff)
}

case class Perceptron(weights: DenseVector[Double], threshold: Double)

object ActivationFunctions {
  val sigmoid: Double => Double = { n: Double =>
    1d / (1d + exp(-n))
  }

  val bipolarSigmoid: Double => Double = { n: Double =>
    2d / (1d + exp(-n)) - 1
  }
}
