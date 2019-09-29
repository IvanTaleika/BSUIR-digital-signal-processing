package by.bsuir.dsp.lab4

import breeze.math._
import breeze.numerics._
import breeze.plot.{Figure, plot}
import by.bsuir.dsp.lab4.filter._
import by.bsuir.dsp.lab4.operation._

object Runner extends App {
  val N = 256
  val points = (0 until N).map(_.toDouble / N)
  val data = (0 until N)
    .map(
      i =>
        sin(120 * i * 2 * math.Pi / N) + sin(60 * i * 2 * math.Pi / N)
          + sin(i * 2 * math.Pi / N))
  val frequencyData = fft(data.map(Complex(_, 0)))

  val f = Figure("lab4")
  val p = f.subplot(3, 3, 0)
  p += plot(points, data, '-')
  p.title = "sin(120x) + sin(60x) + sin(x)"
  val p1 = f.subplot(3, 3, 1)
  p1 += plot(points, frequencyData.map(_.abs), '-')
  p1.title = "Исходная АЧХ"

  val bandstopFilter = blackmanBandstop(data, 4 * N / 5, 50d / N.toDouble)
  val nFilterCoeffs = bandstopFilter.coefficients.size
  val filterPoints = (0 until nFilterCoeffs).map(_.toDouble / nFilterCoeffs)
  val p3 = f.subplot(3, 3, 3)
  p3 += plot(filterPoints, bandstopFilter.coefficients, '-')
  p3.title = "Режекторный - импульсная характеристика"
  val p4 = f.subplot(3, 3, 4)
  p4 += plot(points,
             fft(bandstopFilter.coefficients.map(Complex(_, 0))).map(_.abs),
             '-')
  p4.title = "Режекторный - АЧХ"
  val p5 = f.subplot(3, 3, 5)
  p5 += plot(points,
             fft(bandstopFilter.filter(data).map(Complex(_, 0))).map(_.abs),
             '-')
  p5.title = "АЧХ после режекторного фильтра"

  val chebyshevFilter = new ChebyshevFilter
  val chebyshevData = chebyshevFilter.filter(data)

  val p6 = f.subplot(3, 3, 6)
  p6 += plot(points, chebyshevData, '-')
  p6.title = "Временная хар-ка после фильтра Чебышева"

  val p7 = f.subplot(3, 3, 7)
  p7 += plot(points,
             (0 until N).map(i =>
               sin(60 * i * 2 * math.Pi / N) + sin(i * 2 * math.Pi / N)),
             '-')
  p7.title = "sin(60x) + sin(x)"
}
