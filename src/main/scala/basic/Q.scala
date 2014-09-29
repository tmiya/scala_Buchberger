package basic

import scala.util.matching.Regex

/**
 * @tparam R undelying ring
 * @tparam K this field
 */
trait Field[R, K <: Field[R,K]] extends Ordered[K] {
  /** numer reduced by gcd(numer, denom) */
  val n: R
  /** denom reduced by gcd(numer, denom), should be positive */
  val d: R
  /** inverse for + */
  def unary_- : K
  /** inverse for * */
  def inv : K
  def + (r: K): K
  def - (r: K): K
  def * (r: K): K
  def / (r: K): K
  def isZero: Boolean
  def compare (rhs: K): Int
}

/**
 * Rational number class based on BigInt.
 * @param numer numerator
 * @param denom denominator, should not be zero
 */
case class Q(numer:BigInt, denom:BigInt) extends Field[BigInt, Q] with Ordered[Q] {
  require(denom != BigInt(0), "div by zero.")
  private val g: BigInt = gcd(numer, denom) / denom.signum
  /** numer reduced by gcd(numer, denom) */
  val n: BigInt = numer/g
  /** denom reduced by gcd(numer, denom), should be positive */
  val d: BigInt = denom/g
  def unary_- : Q = Q(-n, d)
  def inv : Q = Q(d*n.signum, n.abs)
  def + (r: Q): Q = Q(n*r.d + r.n*d, d*r.d)
  def - (r: Q): Q = Q(n*r.d - r.n*d, d*r.d)
  def * (r: Q): Q = Q(n*r.n, d*r.d)
  def / (r: Q): Q = Q(n*r.d, d*r.n)
  private def gcd(a: BigInt, b: BigInt): BigInt = {
    if (a<BigInt(0)) gcd(-a, b)
    else if (b<BigInt(0)) gcd(a, -b)
    else if (a==BigInt(0)) b
    else if (b==BigInt(0)) a
    else if (a<b) gcd(a, b%a) 
    else gcd(a%b, b)
  }
  def isZero: Boolean = (n == BigInt(0))
  def compare (rhs: Q): Int = (n*rhs.d) compare (d*rhs.n)
  override def equals(a:Any): Boolean = a.isInstanceOf[Q] && (a match {
      case Q(nn, dd) => n*dd == nn*d
      case _ => false
    })
  override def toString: String = if (d==BigInt(1)) n.toString else s"${n.toString}/${d.toString}"
}

/**
 * Companion object
 */
object Q {
  private val qRegex = """(-?\d+)/(\d+)""".r
  /** string to Q */
  def string2Q(s: String) : Q = s match {
    case qRegex(n,d) => Q(BigInt(n),BigInt(d))
    case _ => throw new NumberFormatException(s)
  }  
}
