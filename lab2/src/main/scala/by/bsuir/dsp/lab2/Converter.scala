package by.bsuir.dsp.lab2

class Converter {
  private var _statistic = 0

  def convolution(xs1: Seq[Double], xs2: Seq[Double]): Seq[Double] = {
    (for (i <- xs1.indices) yield {
      var accum = 0.0
      for (j <- xs1.indices) {
        _statistic += 2
        accum += xs1(j) * (if (j <= i) {
          xs2(i - j)
        } else {
          xs2(xs2.size + i - j)
        })
      }
      accum
    }).map(Utils.roundTwoDecimal)
  }

  def correlation(xs1: Seq[Double], xs2: Seq[Double]): Seq[Double] = {
    (for (i <- xs1.indices) yield {
      var accum = 0.0
      for (j <- i until xs1.size) {
        _statistic += 2
        accum += xs1(xs1.size - j - 1) * xs2(j)
      }
      accum
    }).map(Utils.roundTwoDecimal)
  }

  def statistic: Int = {
    _statistic
  }
}
