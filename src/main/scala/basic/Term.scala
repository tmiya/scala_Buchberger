package basic

/** Term = coefficient + monomial
 * @tparam R underlying ring of K
 * @tparam K field for coeffieient
 * @tparam T type of term 
 */
trait Term[R, K <: Field[R,K], T <: Term[R,K,T]] {
  /** coefficient */
  def k: K
  /** monomial */
  def m: Mono
  /** coefficient = 0 ? */
  def isZero: Boolean
  /** generate term */
  def genTerm(m:Mono, k:K): T
  /** this divides rhs */
  def | (rhs:T): Boolean = this.m | rhs.m
  /** multiply */
  def * (rhs:T): T = genTerm(m*rhs.m, k*rhs.k)
  /** division */
  def / (rhs:T): T = genTerm(m/rhs.m, k/rhs.k)
}

/** Term on Q
 * @param m monomial
 * @param k coefficient
 * @param mo monomial order
 */
case class QTerm(val m: Mono, k: Q)(implicit mo: MonomialOrder) extends Term[BigInt,Q,QTerm] {
    def isZero: Boolean = (k.isZero)
    def genTerm(m:Mono, k:Q): QTerm = QTerm(m, k)   
}
object QTerm {
  /** String to QTerm */
  def string2term(s: String)(implicit mo: MonomialOrder): QTerm = {
    val qmRegex = """(-?\d+)/(\d+)(.*)""".r // q + mono
    val imRegex = """(-?\d+)(.*)""".r // i + mono
    val minusRegex = """-(.*)""".r // - + mono
    s match {
      case qmRegex(n, d, mono) => QTerm(Mono.string2mono(mono), Q(BigInt(n), BigInt(d)))
      case imRegex(n, mono) => QTerm(Mono.string2mono(mono), Q(BigInt(n), BigInt(1)))
      case minusRegex(mono) => QTerm(Mono.string2mono(mono), Q(BigInt(-1), BigInt(1)))
      case _ => QTerm(Mono.string2mono(s), Q(BigInt(1), BigInt(1)))
    }
  }
}