package by.bsuir.dsp.lab4

import scala.math._
package object filter {

  def blackmanBandstop(seq: Seq[Double],
                       m: Int,
                       lowRate: Double): Filter = {
    val lowpass = lowpassBlackmanCoeffs(m, lowRate)
    val highpass = lowToHigh(lowpass)
    Filter((lowpass, highpass).zipped.map(_ + _))
  }


  private def lowpassBlackmanCoeffs(m: Int, passFc: Double): Seq[Double] = {
    val coeffs = for (i <- 0 to m) yield {
      if (i != m / 2) {
        sin(2 * Pi * passFc * (i - m / 2)) /
          (i - m / 2) *
          (0.42 - 0.5 * cos(2 * Pi * i / m) + 0.08 * cos(4 * Pi * i / m))
      } else {
        2 * Pi * passFc
      }
    }
    val sum = coeffs.sum
    coeffs.map(_ / sum)
  }

  private def lowToHigh(coeffs: Seq[Double]): Seq[Double] = {
    for (i <- coeffs.indices) yield {
      if(i % 2 == 0) {
        -coeffs(i)
      } else {
        coeffs(i)
      }
    }
  }
}
