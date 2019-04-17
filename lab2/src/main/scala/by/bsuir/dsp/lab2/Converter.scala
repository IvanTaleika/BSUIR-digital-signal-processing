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
      for (j <- xs1.indices) {
        _statistic += 2
        accum += xs1(j) * (if (j + i < xs1.size) {
          xs2(i + j)
        } else {
          xs2(i + j - xs2.size)
        })
      }
      accum
    }).map(Utils.roundTwoDecimal)
  }

  def statistic: Int = {
    _statistic
  }
}
