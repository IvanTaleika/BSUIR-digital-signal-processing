package by.bsuir.dsp.lab1

import breeze.math.Complex
import breeze.numerics._

import scala.math.Pi

class FastFourierTransform {
  private var _statistic = 0

  def fft(xs: Seq[Complex]): Seq[Complex] = {
    def transform(xs: Seq[Complex]): Seq[Complex] = {
      val n = xs.size
      if (n == 1) {
        xs
      } else {
        val (evenBuff, oddBuff) = evenOdd(xs)
        val even = transform(evenBuff)
        val odd = transform(oddBuff)
        val wn = exp(-Complex.i * (2 * Pi / n))
        var w = Complex(1, 0)
        var ans = Seq.fill(n)(Complex.nan)
        for (i <- 0 until n / 2) {
          ans = ans.updated(i, even(i) + w * odd(i))
          ans = ans.updated(i + n / 2, even(i) - w * odd(i))
          _statistic += 2
          w *= wn
        }
        ans
      }
    }
    transform(xs).map(Utils.roundTwoDecimal)
  }

  def inverseFft(xs: Seq[Complex]): Seq[Complex] = {
    def transform(xs: Seq[Complex]): Seq[Complex] = {
      val n = xs.size
      if (n == 1) {
        xs
      } else {
        val (evenBuff, oddBuff) = evenOdd(xs)
        val even = transform(evenBuff)
        val odd = transform(oddBuff)
        val wn = exp(Complex.i * (2 * Pi / n))
        var w = Complex(1, 0)
        var ans = Seq.fill(n)(Complex.nan)
        for (i <- 0 until n / 2) {
          ans = ans.updated(i, even(i) + w * odd(i))
          ans = ans.updated(i + n / 2, even(i) - w * odd(i))
          _statistic += 2
          w *= wn
        }
        ans
      }
    }
    transform(xs).map(_ / 8).map(Utils.roundTwoDecimal)
  }

  private def evenOdd(xs: Seq[Complex]): (Seq[Complex], Seq[Complex]) = {
    var evenBuff = Seq[Complex]()
    var oddBuff = Seq[Complex]()
    for (i <- xs.indices) {
      if (i % 2 == 0) {
        evenBuff :+= xs(i)
      } else {
        oddBuff :+= xs(i)
      }
    }
    (evenBuff, oddBuff)
  }

  def statistic: Int = {
    _statistic
  }
}
