package by.bsuir.dsp.lab3

object Utils {

  def roundTwoDecimal(d: Double): Double = {
    BigDecimal(d).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble
  }
}
