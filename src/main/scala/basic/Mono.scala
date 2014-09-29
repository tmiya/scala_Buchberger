package basic

/** Monomial
 * @param degs degree for mo.vars, Seq of nat
 * @param mo monomial order for ordering and variable list
 */
case class Mono(val degs: Seq[Int])(implicit mo: MonomialOrder) extends Ordered[Mono] {
  require(degs.forall{_ >= 0}, s"degs must be Seq[nat], but = ${degs}")
  /** compare for Ordered */
  def compare(that: Mono): Int = mo.compare(this, that)
  /** total degree */
  def deg: Int = degs.sum
  override def toString = {
    val vs = mo.vars.map{_.name}.zip(degs).filter{_._2 != 0}.
      map{case (v,d) => if(d==1) v else s"${v}^${d.toString}"}.mkString("")
    if (vs.isEmpty()) "1" else vs
  }
  /** true if this divides rhs */
  def | (rhs:Mono): Boolean = degs.zip(rhs.degs).forall{case (i,j) => i <= j}
  def * (rhs:Mono): Mono = Mono(degs.zip(rhs.degs).map{case (i,j) => i+j})(mo)
  def / (rhs:Mono): Mono = Mono(degs.zip(rhs.degs).map{case (i,j) => i-j})(mo)
}
/** Utility */
object Mono {
  /** least common multiplier */
  def LCM(a: Mono, b:Mono)(implicit mo: MonomialOrder): Mono =
    Mono(a.degs.zip(b.degs).map{case (i,j) => scala.math.max(i,j)})
  private val monoRegex = """(\w)(\^\d+)?""".r // monomial
  /** string to Mono */
  def string2mono(s: String)(implicit mo: MonomialOrder): Mono = {
    val vs: Seq[String] = monoRegex.findAllIn(s).toSeq
    val ds: Map[Symbol,Int] = vs.map{v => v.split('^').toList match {
      case sym::deg::Nil => (Symbol(sym), deg.toInt)
      case sym::Nil => (Symbol(sym), 1)
      case _ => throw new NumberFormatException(s)
    }}.toMap
    Mono(mo.vars.map{sym => ds.getOrElse(sym,0)})(mo)
  }
}
