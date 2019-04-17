package by.bsuir.dsp.lab1

import breeze.math._
import breeze.numerics._
import breeze.plot.{Figure, plot}

// y=cos(x)+sin(x) БПФ с прореживанием по времени 8
object Runner extends App {
  val N = 8
  val points = (0 until N).map(_ * 2 * math.Pi / N)
  val data = (0 until N)
//    .map(i => cos(4 * 3 * i * 2 * math.Pi / N) + sin(i * 2 * math.Pi / N))
    .map(i => cos(i * 2 * math.Pi / N) + sin(i * 2 * math.Pi / N))
    .map(Complex(_, 0))

  println(s"source $data")
  val fast = new FastFourierTransform
  val discrete = new DiscreteFourierTransform
  val fftData = fast.fft(data)
  val dftData = discrete.dft(data)
  val inverseFft = fast.inverseFft(fftData)
  val inverseDft = discrete.inverseDft(dftData)
  println(s"fft $fftData")
  println(s"dft $dftData")
  println(s"inverseFft $inverseFft")
  println(s"inverseDft $inverseDft")

  val f = Figure()
  val p = f.subplot(3, 3, 0)
  p += plot(points, data.map(_.real), '-')
  p.title = "Source sampling"
  val p3 = f.subplot(3, 3, 3)
  p3 += plot(points, dftData.map(_.abs), '-')
  p3.title = "DFT magnitude"
  val p4 = f.subplot(3, 3, 4)
  p4 += plot(points, dftData.map(n => atan2(n.imag, n.real)), '-')
  p4.title = "DFT phase"
  val p5 = f.subplot(3, 3, 5)
  p5 += plot(points, inverseDft.map(_.real), '-')
  p5.title = "inverse DFT"

  val p6 = f.subplot(3, 3, 6)
  p6 += plot(points, fftData.map(_.abs), '-')
  p6.title = "FFT magnitude"
  val p7 = f.subplot(3, 3, 7)
  p7 += plot(points, dftData.map(n => atan2(n.imag, n.real)), '-')
  p7.title = "FFT phase"
  val p8 = f.subplot(3, 3, 8)
  p8 += plot(points, inverseFft.map(_.real), '-')
  p8.title = "inverse FFT"

  println(s"Number of operation in FFT: ${fast.statistic}")
  println(s"Number of operation in DFT: ${discrete.statistic}")
}
