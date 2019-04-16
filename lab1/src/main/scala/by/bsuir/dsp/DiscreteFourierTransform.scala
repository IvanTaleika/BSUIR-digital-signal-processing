package by.bsuir.dsp

import breeze.math.Complex
import breeze.numerics._

import scala.math.Pi

object DiscreteFourierTransform {

  def dft(xs: Seq[Double]): Seq[Complex] = {
    xs.indices.map(i => {
      val precize = xs.indices
        .map(j => xs(j) * exp(-Complex.i * (2 * Pi / xs.size) * i * j))
        .sum
      // TODO: do we need round?
      Complex(round(precize.real), round(precize.imag))
    })

  }
}
