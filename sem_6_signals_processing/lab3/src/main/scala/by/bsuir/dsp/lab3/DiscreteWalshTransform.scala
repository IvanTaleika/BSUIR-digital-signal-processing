package by.bsuir.dsp.lab3

import breeze.linalg.DenseMatrix

class DiscreteWalshTransform(val n: Int) {
  private var _statistic = 0

  val matrix: DenseMatrix[Int] = DenseMatrix(
    (1, 1, 1, 1, 1, 1, 1, 1),       //0
    (1, 1, 1, 1, -1, -1, -1, -1),   //1
    (1, 1, -1, -1, -1, -1, 1, 1),   //2
    (1, 1, -1, -1, 1, 1, -1, -1),   //3
    (1, -1, -1, 1, 1, -1, -1, 1),   //4
    (1, -1, -1, 1, -1, 1, 1, -1),   //5
    (1, -1, 1, -1, -1, 1, -1, 1),   //6
    (1, -1, 1, -1, 1, -1, 1, -1),   //7
  )


  def dwt(xs: Seq[Double]): Seq[Double] = {
    transform(xs).map(_ / 8).map(Utils.roundTwoDecimal)
  }

  def inverseDwt(xs: Seq[Double]): Seq[Double] = {
    transform(xs)
  }

  private def transform(xs: Seq[Double]): Seq[Double] = {
    for (i <- xs.indices) yield {
      _statistic += xs.size -1
      (for (j <- xs.indices) yield {
        _statistic += 1
        xs(j) * matrix(i,j)
      }).sum
    }
  }

  def statistic: Int = {
    _statistic
  }
}
