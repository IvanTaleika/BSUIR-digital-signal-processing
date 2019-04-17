package by.bsuir.dsp.lab3

import breeze.numerics._
import breeze.plot.{Figure, plot}

// y=cos(x)+sin(x) БПФ с прореживанием по времени 8
object Runner extends App {

  val N = 8
  val points = (0 until N).map(_ * 2 * math.Pi / N)
  val data = (0 until N)
    .map(i => cos(i * 2 * math.Pi / N) + sin(i * 2 * math.Pi / N))

  println(s"source $data")
  val fast = new FastWalshTransform
  val discrete = new DiscreteWalshTransform(N)
  val fwtData = fast.fwt(data)
  val dwtData = discrete.dwt(data)
  val inverseFwt = fast.inverseFwt(fwtData)
  val inverseDwt = discrete.inverseDwt(dwtData)
  println(s"fwt $fwtData")
  println(s"dwt $dwtData")
  println(s"inverseFwt $inverseFwt")
  println(s"inverseDwt $inverseDwt")

  val f = Figure("lab4")
  val p = f.subplot(3, 2, 0)
  p += plot(points, data, '-')
  p.title = "Source sampling"


  val p2 = f.subplot(3, 2, 2)
  p2 += plot(points, dwtData, '-')
  p2.title = "DWT"
  val p3 = f.subplot(3, 2, 3)
  p3 += plot(points, inverseDwt, '-')
  p3.title = "inverse DWT"

  val p4 = f.subplot(3, 2, 4)
  p4 += plot(points, fwtData, '-')
  p4.title = "FWT"
  val p5 = f.subplot(3, 2, 5)
  p5 += plot(points, inverseFwt, '-')
  p5.title = "inverse FWT"

  println(s"Number of operation in FWT: ${fast.statistic}")
  println(s"Number of operation in DWT: ${discrete.statistic}")
}
