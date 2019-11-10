package by.bsuir.dip

import breeze.plot._

object Runner extends App {
  val samples = NoiseFilter.createSample(n = 3)
  val net = new KohonenNetModel().train(Images.ImagesWithNames)
  val results = samples.map {
    case(noiseLevel, sample) =>
      val nRightPredictions = sample.foldLeft(0) {
        case (rightPredictionsCount, (name, image)) =>
          val (resultImageName, _) = net.fit(image)
          println(s"$name is '$resultImageName'")
          val isRight = if(name(0) == resultImageName(0)) 1 else 0
          rightPredictionsCount + isRight
      }
    noiseLevel * 100 -> nRightPredictions.toDouble / sample.size * 100
  }

  val f = Figure()
  val p = f.subplot(0)
  p += plot(results.map(_._1), results.map(_._2))
  p.xlabel = "noise percent"
  p.ylabel = "recognition percent"
}
