package basic

import scala.math.Ordering

trait MonomialOrder extends Ordering[Mono]{
  val vars: Seq[Symbol]
  def compare(x: Mono, y: Mono): Int
}
case class LexOrder(val vars: Seq[Symbol]) extends MonomialOrder {
  def compare(xs: Seq[Int], ys: Seq[Int]): Int = {
    if (xs.isEmpty || ys.isEmpty) 0
    else {
      val c = xs.head compare ys.head
      if (c!=0) c else compare(xs.tail, ys.tail)
    }    
  }
  def compare(x: Mono, y: Mono): Int = compare(x.degs, y.degs)
}
case class GrLexOrder(val vars: Seq[Symbol]) extends MonomialOrder {
  val lexOrder = LexOrder(vars)
  def compare(x: Mono, y: Mono): Int = {
    val c = x.deg compare y.deg
    c match {
      case 0 => lexOrder.compare(x, y)
      case _ => c
    }
  }
}

case class GRevLexOrder(val vars: Seq[Symbol]) extends MonomialOrder {
  val lexOrder = LexOrder(vars.reverse)
  def compare(x: Mono, y: Mono): Int = {
    val c = x.deg compare y.deg
    c match {
      case 0 => -lexOrder.compare(x, y)
      case _ => c
    }
  }
}
case class Mono(val degs: Seq[Int])(implicit mo: MonomialOrder) extends Ordered[Mono] {
  def compare(that: Mono): Int = mo.compare(this, that)
  def deg: Int = degs.sum
  override def toString = {
    val vs = mo.vars.map{_.name}.zip(degs).filter{_._2 != 0}.
      map{case (v,d) => if(d==1) v else s"${v}^${d.toString}"}.mkString("")
    if (vs.isEmpty()) "1" else vs
  }
  def | (rhs:Mono): Boolean = degs.zip(rhs.degs).forall{case (i,j) => i <= j}
  def * (rhs:Mono): Mono = Mono(degs.zip(rhs.degs).map{case (i,j) => i+j})(mo)
  def / (rhs:Mono): Mono = Mono(degs.zip(rhs.degs).map{case (i,j) => i-j})(mo)
}
object Mono {
  def LCM(a: Mono, b:Mono)(implicit mo: MonomialOrder): Mono =
    Mono(a.degs.zip(b.degs).map{case (i,j) => scala.math.max(i,j)})
}
