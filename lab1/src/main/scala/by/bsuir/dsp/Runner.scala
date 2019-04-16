package by.bsuir.dsp

import breeze.numerics._
import breeze.plot._

// y=cos(x)+sin(x) БПФ с прореживанием по времени 8
object Runner extends App {
  val N = 8
  val points = (0 until 8).map(_ * 2 * math.Pi / N)
  val data = (0 until 8)
    .map(i => cos(i * 2 * math.Pi / N) + sin(i * 2 * math.Pi / N))
  println(data)

  val dftData = DiscreteFourierTransform.dft(data)
  println(dftData)
  val f = Figure()
  val p = f.subplot(3, 2, 0)
  p += plot(points, data, '-')
  p.title = "Source sampling"

  val p1 = f.subplot(3, 2, 2)
  p1 += plot(points, dftData.map(_.abs), '-')
  p1.title = "DFT amplitude"

}
