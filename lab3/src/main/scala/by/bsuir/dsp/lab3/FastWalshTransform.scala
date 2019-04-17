package by.bsuir.dsp.lab3

class FastWalshTransform {
  private var _statistic = 0

  def fwt(xs: Seq[Double]): Seq[Double] = {
    def transform(xs: Seq[Double]): Seq[Double] = {
      val n = xs.size
      if (n == 1) {
        xs
      } else {
        var first = Seq[Double]()
        var second = Seq[Double]()
        for (i <- 0 until n / 2) {
          first = first :+ xs(i) + xs(n / 2 + i)
          second = second :+ xs(i) - xs(n / 2 + i)
          _statistic += 2
        }
        transform(first) ++ transform(second)
      }
    }
    transform(xs).map(_ / xs.size).map(Utils.roundTwoDecimal)
  }

  def inverseFwt(xs: Seq[Double]): Seq[Double] = {
    def transform(xs: Seq[Double]): Seq[Double] = {
      val n = xs.size
      if (n == 1) {
        xs
      } else {
        val first = transform(xs.take(n / 2))
        val second = transform(xs.takeRight(n / 2))
        var res = Seq.fill(n)(0.0)
        for (i <- 0 until n / 2) {
          res = res.updated(i, first(i) + second(i))
          res = res.updated(i + n / 2, first(i) - second(i))
          _statistic += 2
        }
        res
      }
    }
    transform(xs)
  }

  def statistic: Int = {
    _statistic
  }
}
