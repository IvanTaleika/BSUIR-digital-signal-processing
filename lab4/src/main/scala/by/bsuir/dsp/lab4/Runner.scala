package by.bsuir.dsp.lab4

import breeze.math._
import breeze.numerics._
import breeze.plot.{Figure, plot}
import by.bsuir.dsp.lab4.filter._
import by.bsuir.dsp.lab4.operation._

// [0.2, 0.3]
// средние частоты на 60 и 68
// частота - 128, убрать полосу [54,74]
object Runner extends App {
  val N = 256
//  val temp = 0 until 1 by 1/N
  val points = (0 until N).map(_.toDouble / N)
  val data = (0 until N)
    .map(
      i =>
        sin(120 * i * 2 * math.Pi / N) + sin(60 * i * 2 * math.Pi / N)
          + sin(i * 2 * math.Pi / N))
  val frequencyData = fft(data.map(Complex(_, 0)))

  //  val discrete = new DiscreteFourierTransform
  //  val dftData = discrete.dft(data)
  //  val inverseFft = fast.inverseFft(fftData)
  //  val inverseDft = discrete.inverseDft(dftData)
//  println(fft(data.map(Complex(_, 0))).map(_.abs).zipWithIndex.filter(_._1 != 0))
  val f = Figure("lab4")
  val p = f.subplot(3, 3, 0)
  p += plot(points, data, '-')
  p.title = "sin(120x) + sin(60x) + sin(x)"
  val p1 = f.subplot(3, 3, 1)
  p1 += plot(points, frequencyData.map(_.abs), '-')
  p1.title = "Исходная амплитудно-частотная"

  val bandstopFilter = blackmanBandstop(data, 4 * N / 5, 50d / N.toDouble)
  val nFilterCoeffs = bandstopFilter.coefficients.size
  val filterPoints = (0 until nFilterCoeffs).map(_.toDouble / nFilterCoeffs)
  val p3 = f.subplot(3, 3, 3)
  p3 += plot(filterPoints, bandstopFilter.coefficients, '-')
  p3.title = "Режекторный - импульсная"
  val p4 = f.subplot(3, 3, 4)
  p4 += plot(points,
             fft(bandstopFilter.coefficients.map(Complex(_, 0))).map(_.abs),
             '-')
  p4.title = "Режекторный - амплитудна-частотная"
  val p5 = f.subplot(3, 3, 5)
  p5 += plot(points,
             fft(bandstopFilter.filter(data).map(Complex(_, 0))).map(_.abs),
             '-')
  p5.title = "Фильтрованная амплитудно-частотная"

  val chebyshevFilter = new ChebyshevFilter
  val chebyshevData = chebyshevFilter.filter(data)
  //  val lib = filterLP(DenseVector(data.toArray), 0.9, 2d, 128).data
  //  val p4 = f.subplot(3, 2, 4)
  //  p4 += plot(points, lib, '-')
  //  p4.title = "DFT phase"
  //  val p5 = f.subplot(3, 2, 5)
  //  p5 += plot(points, fft(lib.toSeq.map(Complex(_, 0))).map(_.abs), '-')
  //  p5.title = "inverse DFT"
//    val p6 = f.subplot(3, 3, 6)
//    p6 += plot(points, frequencyData.map(_.abs), '-')
//    p6.title = "Фильтрованная временная"
  val p7 = f.subplot(3, 3, 7)
  p7 += plot(points, chebyshevData, '-')
  p7.title = "Фильтрованная временная"
//  val p8 = f.subplot(3, 3, 8)
//  p8 += plot(points, fft(chebyshevData.map(Complex(_, 0))).map(_.real), '-')
//  p8.title = "Фильтрованная частотная"
  val p8 = f.subplot(3, 3, 8)
  p8 += plot(points,
             (0 until N).map(i =>
               sin(60 * i * 2 * math.Pi / N) + sin(i * 2 * math.Pi / N)),
             '-')
  p8.title = "sin(60x) + sin(x)"

}
