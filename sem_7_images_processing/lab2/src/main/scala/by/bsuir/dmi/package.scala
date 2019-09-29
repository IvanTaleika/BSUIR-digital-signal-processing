package by.bsuir

package object dmi {

  implicit object attributesOrdering extends Ordering[Attributes] {
    override def compare(x: Attributes, y: Attributes): Int = x.module.compare(y.module)
  }
}
