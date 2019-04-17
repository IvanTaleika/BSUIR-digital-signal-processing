package by.bsuir.dsp.lab2

import breeze.numerics.{cos, sin}
import breeze.plot.{Figure, plot}

// TODO: statistic recalculate
object Runner extends App {
  val N = 8
  val points = (0 until 8).map(_ * 2 * math.Pi / N)
  val xs1 = (0 until 8)
    .map(i => cos(i * 2 * math.Pi / N))
  val xs2 = (0 until 8)
    .map(i => sin(i * 2 * math.Pi / N))

  println(s"cos(x) $xs1")
  println(s"sin(x) $xs2")

  val fastConverter = new FastFourierConverter
  val converter = new Converter

  val slowConvolution = converter.convolution(xs1, xs2)
  val fastConvolution = fastConverter.convolution(xs1, xs2)
  println(s"slow convolution $slowConvolution")
  println(s"fast convolution $fastConvolution")

  val slowCorrelation = converter.correlation(xs1, xs2)
  val fastCorrelation = fastConverter.correlation(xs1, xs2)
  println(s"slow correlation $slowCorrelation")
  println(s"fast correlation $fastCorrelation")

  val f = Figure()
  val p = f.subplot(3, 2, 0)
  p += plot(points, xs1, '-')
  p += plot(points, xs2, '-')
  p.title = "Source sampling"

  val p2 = f.subplot(3, 2, 2)
  p2 += plot(points, slowConvolution, '-')
  p2.title = "Slow convolution"
  val p3 = f.subplot(3, 2, 3)
  p3 += plot(points, slowCorrelation, '-')
  p3.title = "Slow correlation"

  val p4 = f.subplot(3, 2, 4)
  p4 += plot(points, fastConvolution, '-')
  p4.title = "Fast convolution"
  val p5 = f.subplot(3, 2, 5)
  p5 += plot(points, fastCorrelation, '-')
  p5.title = "Fast correlation"

  println(s"Number of operation in FFT: ${converter.statistic}")
  println(s"Number of operation in DFT: ${fastConverter.statistic}")
}
