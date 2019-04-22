package by.bsuir.dsp.lab4.filter

import by.bsuir.dsp.lab4.operation._

case class Filter(coefficients: Seq[Double]) {
  def filter(xs: Seq[Double]): Seq[Double] = {
    convolution(xs, coefficients)
  }
}
