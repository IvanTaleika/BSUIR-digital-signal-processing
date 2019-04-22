package by.bsuir.dsp.lab4.filter

//НЧ 0.5%, 6 полюсов, 0.25 f
class ChebyshevFilter {
  val aSeq: Seq[Double] = Seq(1.434449e-02, 8.606697e-02, 2.151674e-01, 2.868899e-01,
    2.151674e-01, 8.606697e-02, 1.434449e-02)

  val bSeq: Seq[Double] = Seq(1.076052, -1.662847, 1.191063, -7.403087e-01, 2.752158e-01,
    -5.722251e-02)

  def filter(xs: Seq[Double]): Seq[Double] = {
    var result = Seq[Double]()
    for (i <- xs.indices) {
      result = result :+ (i match {
        case 0 => xs(0) * aSeq(0)
        case 1 => xs(1) * aSeq(0) + xs(0) * aSeq(1) + result(0) * bSeq(0)
        case 2 =>
          xs(2) * aSeq(0) + xs(1) * aSeq(1) + xs(0) * aSeq(2) +
            result(1) * bSeq(0) + result(0) * bSeq(1)
        case 3 =>
          xs(3) * aSeq(0) + xs(2) * aSeq(1) + xs(1) * aSeq(2) + xs(0) * aSeq(3) +
            result(2) * bSeq(0) + result(1) * bSeq(1) + result(0) * bSeq(2)
        case 4 =>
          xs(4) * aSeq(0) + xs(3) * aSeq(1) + xs(2) * aSeq(2) + xs(1) * aSeq(3) +
            xs(0) * aSeq(4) + result(3) * bSeq(0) + result(2) * bSeq(1) +
            result(1) * bSeq(2) + result(0) * bSeq(3)
        case _ =>
          xs(i) * aSeq(0) + xs(i - 1) * aSeq(1) + xs(i - 2) * aSeq(2) +
            xs(i - 3) * aSeq(3) + xs(i - 4) * aSeq(4) + xs(i - 5) * aSeq(5) +
            result(i - 1) * bSeq(0) + result(i - 2) * bSeq(1) +
            result(i - 3) * bSeq(2) + result(i - 4) * bSeq(3) + result(i-5) * bSeq(
            4)
      })
    }
    result
  }
}
