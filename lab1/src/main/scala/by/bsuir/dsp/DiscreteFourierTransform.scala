package by.bsuir.dsp

import breeze.math.Complex
import breeze.numerics._

import scala.math.Pi

class DiscreteFourierTransform {
  private var _statistic = 0

  def dft(xs: Seq[Complex]): Seq[Complex] = {
    xs.indices.map(i => {
      _statistic += xs.size * xs.size // multiplications
      _statistic += xs.size * (xs.size - 1) // additions
      xs.indices
        .map(j => xs(j) * exp(-Complex.i * (2 * Pi / xs.size) * i * j))
        .sum
    })
  }
  def inverseDft(xs: Seq[Complex]): Seq[Complex] = {
    xs.indices.map(i => {
      xs.indices
        .map(j => xs(j) * exp(Complex.i * (2 * Pi / xs.size) * i * j))
        .sum / xs.size
    })
  }

  def statistic: Int = {
    _statistic
  }
}
