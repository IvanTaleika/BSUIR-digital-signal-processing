package by.bsuir.dsp.lab4

import breeze.math.Complex

package object utils {
  def roundTwoDecimal(c: Complex): Complex = {
    Complex(roundTwoDecimal(c.real), roundTwoDecimal(c.imag))
  }
  def roundTwoDecimal(d: Double): Double = {
    BigDecimal(d).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble
  }
}
