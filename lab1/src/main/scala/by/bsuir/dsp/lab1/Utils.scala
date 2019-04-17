package by.bsuir.dsp.lab1

import breeze.math.Complex

object Utils {
  def roundTwoDecimal(c: Complex): Complex = {
    Complex(roundTwoDecimal(c.real), roundTwoDecimal(c.imag))
  }
  def roundTwoDecimal(d: Double): Double = {
    BigDecimal(d).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble
  }
}
