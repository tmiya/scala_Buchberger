package basic

case class Q(numer:Int, denom:Int) {
  private val g: Int = gcd(numer, denom) / denom.signum
  val n: Int = numer/g
  val d: Int = denom/g
  def unary_- : Q = Q(-n, d)
  def inv : Q = Q(d*n.signum, n.abs)
  def + (r: Q): Q = Q(n*r.d + r.n*d, d*r.d)
  def - (r: Q): Q = Q(n*r.d - r.n*d, d*r.d)
  def * (r: Q): Q = Q(n*r.n, d*r.d)
  def / (r: Q): Q = Q(n*r.d, d*r.n)
  def gcd(a: Int, b: Int): Int = {
    if (a<0) gcd(-a, b)
    else if (b<0) gcd(a, -b)
    else if (a==0) b
    else if (b==0) a
    else if (a<b) gcd(a, b%a) 
    else gcd(a%b, b)
  }
  override def equals(a:Any): Boolean = a.isInstanceOf[Q] && (a match {
      case q@Q(nn, dd) => n*q.d == q.n*d
      case _ => false
    })
  override def toString: String = if (d==1) n.toString else s"${n.toString}/${d.toString}"
}
