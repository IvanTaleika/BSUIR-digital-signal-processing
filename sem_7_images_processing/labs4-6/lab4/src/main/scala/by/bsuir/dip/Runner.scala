package by.bsuir.dip

object Runner extends App {
  val sample = NoiseFilter.createSample()
  val net = new MultilayerNetModel().train(Images.ImagesWithNames)
  sample.foreach{ case (n, i) =>
    val res = net.fit(i)
    println(s"$n:")
    res.foreach{
      case (percent, (resultName , _)) =>
        println(s"Is $percent% equals to '$resultName'")
    }
    println()

  }
}
