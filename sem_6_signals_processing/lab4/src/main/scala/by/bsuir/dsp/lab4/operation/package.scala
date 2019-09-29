package by.bsuir.dsp.lab4

import breeze.math.Complex
import breeze.numerics._
import by.bsuir.dsp.lab4.utils._

import scala.math.Pi

package object operation {

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
        var ans = Seq.fill(n)(Complex.zero)
        for (i <- 0 until n / 2) {
          ans = ans.updated(i, even(i) + w * odd(i))
          ans = ans.updated(i + n / 2, even(i) - w * odd(i))
          w *= wn
        }
        ans
      }
    }
    transform(toPowerOfTwoSize(xs)).map(roundTwoDecimal)
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
          w *= wn
        }
        ans
      }
    }

    transform(toPowerOfTwoSize(xs)).map(_ / xs.size).map(roundTwoDecimal)
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

  private def toPowerOfTwoSize(xs: Seq[Complex]): Seq[Complex] = {
    val power = log2(xs.size)
    if (power.isValidInt) {
      xs
    } else {
      xs ++ Seq.fill(pow(2, ceil(power)).toInt - xs.size)(Complex.zero)
    }
  }

  def convolution(xs1: Seq[Double], xs2: Seq[Double]): Seq[Double] = {
    val z1 = fft(xs1.map(Complex(_, 0)))
    val z2 = fft(xs2.map(Complex(_, 0)))
    val z = for (i <- z1.indices) yield {
      z1(i) * z2(i)
    }
    inverseFft(z).map(_.real)
  }

}
