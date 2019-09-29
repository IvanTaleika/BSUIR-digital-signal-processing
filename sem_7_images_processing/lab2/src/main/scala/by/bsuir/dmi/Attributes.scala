package by.bsuir.dmi
import scala.math._
case class Attributes(area: Double, perimeter: Double, centerMass: Double, elongation: Double) {
  def module: Double = sqrt(pow(area, 2) + pow(perimeter, 2) + pow(centerMass, 2) + pow(elongation, 2))
  def -(a: Attributes) =
    Attributes(area - a.area, perimeter - a.perimeter, centerMass - a.centerMass, elongation - a.elongation)
  def +(a: Attributes) =
    Attributes(area + a.area, perimeter + a.perimeter, centerMass + a.centerMass, elongation + a.elongation)
}
