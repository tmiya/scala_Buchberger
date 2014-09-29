package basic

import scala.collection.mutable.{Map => MMap, ArrayBuffer => MArray, Set => MSet}
import scala.util.matching.Regex

/** Polynomial
 * @tparam R underlying ring for K
 * @tparam K field for coefficient
 * @tparam T term
 * @tparam P this polynomial  
 */
trait Poly[R, K <: Field[R,K], T <: Term[R,K,T], P <: Poly[R,K,T,P]] {
  self:P =>
  /** class manifest for P */
  val manifestP: ClassManifest[P]
  /** generate Term from m and k
   * @param m monomial
   * @param k coefficient
   */
  def genTerm(m: Mono, k:K): T
  /** generate Poly from Terms
   * @param ts sequence of Term
   */
  def genPoly(ts: Seq[T]): P
  /** raw args for constructor */
  val rawTerms: Seq[T]
  /** monomial order */
  val mo: MonomialOrder
  /** terms, filtered by nonzero coefficient, and ordered (first = highest) */
  val ts: Seq[T] = rawTerms.filter{!_.isZero}.sortWith{case (cx, cy) => cy.m < cx.m}
  /** string representation */
  override def toString: String = 
    if (isZero) "0" 
    else ts.map{case t => {
      val (cs, ms) = (t.k.toString, t.m.toString)
      if (ms == "1") cs
      else if (cs == "1") ms
      else if (cs == "-1") "-"+ms
      else cs+ms
    }}.mkString(" + ")
  /** equality is defined by ts */
  override def equals(a:Any): Boolean = a.isInstanceOf[P] && (ts == a.asInstanceOf[P].ts)
  /** leading term */
  def LT: T = ts.head
  /** leading coefficient */
  def LC: K = ts.head.k
  /** leading monomial */
  def LM: Mono = ts.head.m
  /** is zero-polynomial */
  def isZero: Boolean = ts.isEmpty
  /** zero polynomial */
  val zero: P

  // internal functions for polynomial operations 
  private def ts2mmap: MMap[Mono,K] = MMap(ts.map{t => (t.m, t.k)}: _*)
  private def mmap2ts(map: MMap[Mono,K]): Seq[T] = map.toSeq.map{t => genTerm(t._1, t._2)}
  private def add2mmap(map: MMap[Mono,K], m:Mono, k:K): Unit = {
    map.get(m) match {
      case None => map.put(m, k)
      case Some(kk) => {val sum = kk + k; if (sum.isZero) map.remove(m) else map(m) = sum}
    }
  }
  def + (m: Mono, k: K): P = {
    val map: MMap[Mono,K] = ts2mmap
    add2mmap(map, m, k)
    genPoly(mmap2ts(map))
  }
  def - (m: Mono, k:K): P = this + (m, -k)
  def + (t: T): P = this + (t.m, t.k)
  def - (t: T): P = this + (t.m, -t.k)
  def + (rhs: P): P = {
    val map: MMap[Mono,K] = ts2mmap
    rhs.ts.foreach{t => add2mmap(map, t.m, t.k)}
    genPoly(mmap2ts(map))
  }
  def - (rhs: P): P = {
    val map: MMap[Mono,K] = ts2mmap
    rhs.ts.foreach{t => add2mmap(map, t.m, -t.k)}
    genPoly(mmap2ts(map))
  }
  def * (k:K): P = 
    if (k.isZero) zero else genPoly(ts.map{t => genTerm(t.m, t.k*k)})
  def * (m:Mono, k:K): P = 
    if (k.isZero) zero else genPoly(ts.map{t => genTerm(t.m*m, t.k*k)})
  def * (t: T): P = this * (t.m, t.k)
  def * (rhs: P): P = {
    val map: MMap[Mono,K] = MMap.empty
    for(t <- this.ts; rt <- rhs.ts) yield {add2mmap(map, t.m*rt.m, t.k*rt.k)}
    genPoly(mmap2ts(map))
  }
  def / (k:K): P = genPoly(ts.map{t => genTerm(t.m, t.k/k)})
  def / (m:Mono, k:K): P = 
    if (k.isZero) throw new ArithmeticException("div by zero") 
    else genPoly(ts.map{case t => genTerm(t.m/m, t.k/k)})
  def / (t: T): P = this / (t.m, t.k)
  def divmod(fs: Seq[P]):(Seq[P], P) = {
    if (ts.isEmpty) (fs.map{f => genPoly(Nil)}, genPoly(Nil)) 
    else {
      val s:Int = fs.length
      var r:P = genPoly(Nil)
      val a:Array[P] = Array.fill(fs.length)(zero)(manifestP)
      var p:P = this
      while(!p.isZero) {
        var i = 0
        var div_occur = false
        while(i < s && !div_occur) {
          if (fs(i).LT | p.LT) {
            val t = p.LT / fs(i).LT
            a(i) = a(i) + t
            p = p - fs(i) * t
            div_occur = true
          } else {i += 1}
        }
        if (!div_occur) {
          r = r + p.LT
          p = p - p.LT
        }
      }
      (a.toSeq, r)
    }
  }
  def bar(fs: Seq[P]): P = divmod(fs)._2
}


case class QPoly(rawTerms: Seq[QTerm])(implicit monoOrd: MonomialOrder) extends Poly[BigInt,Q,QTerm,QPoly] {
  val manifestP = classManifest[QPoly]
  val mo: MonomialOrder = monoOrd
  lazy val zero: QPoly = QPoly(Nil)
  def genTerm(m: Mono, k:Q): QTerm = QTerm(m, k)
  def genPoly(qts: Seq[QTerm]): QPoly = QPoly(qts)
}

object Poly {  
  def S[R, K<:Field[R,K], T<:Term[R,K,T], P<:Poly[R,K,T,P]](f:P, g:P)(implicit mo: MonomialOrder): P = {
    val xg = Mono.LCM(f.LM, g.LM)
    f * (xg / f.LM, f.LC.inv) - g * (xg / g.LM, g.LC.inv)
  }
  def buchberger[R, K<:Field[R,K], T<:Term[R,K,T], P<:Poly[R,K,T,P]](fs: Seq[P])(implicit mo: MonomialOrder): Seq[P] = {
    var gs: MArray[P] = MArray[P](fs:_*)
    var cont = true
    while(cont) {
      val gss = MArray[P](gs:_*)
      val nss = gss.length
      var found = false
      var i = 0
      while(!found && i<nss) {
        val p = gss(i)
        var j = i+1
        while(!found && j<nss) {
          val q = gss(j)
          val s = Poly.S[R,K,T,P](p,q).bar(gss)
          if (!s.isZero) {gs += s; found = true}
          j += 1
        }
        i += 1
      }
      if (gs.length == gss.length) {cont = false}
    }
    gs.toSeq
  }
  def reduce[R, K<:Field[R,K], T<:Term[R,K,T], P<:Poly[R,K,T,P]](fs: Seq[P])(implicit mo: MonomialOrder): Seq[P] = {
    def red(ps: List[P], acc:List[P]): List[P] = ps match {
      case Nil => acc
      case p::pss => 
        if (pss.exists{pi => pi.LM | p.LM} || acc.exists{pi => pi.LM | p.LM}) red(pss,acc) 
        else red(pss, p::acc)
    }
    red(fs.toList, Nil).reverse.map{p => p / p.LC}
  }
  def buchberger2[R, K<:Field[R,K], T<:Term[R,K,T], P<:Poly[R,K,T,P]](fs: Seq[P])(implicit mo: MonomialOrder): Seq[P] = {
    val s: Int = fs.length
    val b: MSet[(Int,Int)] = MSet.empty
    for(i <- Range(0,s); j <- Range(i+1, s)) yield {b += ((i,j))}
    val gs: MArray[P] = MArray[P](fs:_*)
    var t: Int = s
    //
    def criterion(i:Int, j:Int): Boolean = {
      val lcm = Mono.LCM(gs(i).LM, gs(j).LM)
      Range(0, t).exists{k =>
        (k!=i) && (k!=j) && (!b.contains((i,k))) && (!b.contains((k,i))) && (!b.contains((j,k))) && (!b.contains((k,j))) &&
        (gs(k).LM | lcm)
      }
    }
    //
    while(!b.isEmpty) {
      val (i, j) = b.head
      val fi = gs(i)
      val fj = gs(j)
      if ((Mono.LCM(fi.LM, fj.LM) != (fi.LM * fj.LM)) && (!criterion(i,j))) {
        val spair = Poly.S[R,K,T,P](fi, fj).bar(gs)
        if (!spair.isZero) {
          t = t+1
          gs += spair
          for(ii <- Range(0, t-1)) yield {b += ((ii,t-1))}
        }
      }
      b -= ((i,j))
    }    
    gs.toSeq
  }
  def buchberger3[R, K<:Field[R,K], T<:Term[R,K,T], P<:Poly[R,K,T,P]](fs: Seq[P])(implicit mo: MonomialOrder): Seq[P] = {
    val s: Int = fs.length
    val b: MSet[(Int,Int)] = MSet.empty
    for(i <- Range(0,s); j <- Range(i+1, s)) yield {b += ((i,j))}
    val gs: MArray[P] = MArray[P](fs:_*)
    var t: Int = s
    //
    def criterion(i:Int, j:Int): Boolean = {
      val lcm = Mono.LCM(gs(i).LM, gs(j).LM)
      Range(0, t).exists{k =>
        (k!=i) && (k!=j) && (!b.contains((i,k))) && (!b.contains((k,i))) && (!b.contains((j,k))) && (!b.contains((k,j))) &&
        (gs(k).LM | lcm)
      }
    }
    //
    while(!b.isEmpty) {
      val (i, j) = b.toSeq.sortBy{ij => Mono.LCM(gs(ij._1).LM, gs(ij._2).LM).deg}.head
      val fi = gs(i)
      val fj = gs(j)
      if ((Mono.LCM(fi.LM, fj.LM) != (fi.LM * fj.LM)) && (!criterion(i,j))) {
        val spair = Poly.S[R,K,T,P](fi, fj).bar(gs.sortBy{g => g.LM.deg})
        if (!spair.isZero) {
          t = t+1
          gs += spair
          for(ii <- Range(0, t-1)) yield {b += ((ii,t-1))}
        }
      }
      b -= ((i,j))
    }    
    gs.toSeq
  }
}
object QPoly {
  def S(f:QPoly, g:QPoly)(implicit mo: MonomialOrder): QPoly =
    Poly.S[BigInt,Q,QTerm,QPoly](f, g)(mo)
  def buchberger(fs: Seq[QPoly])(implicit mo: MonomialOrder): Seq[QPoly] =
    Poly.buchberger[BigInt,Q,QTerm,QPoly](fs)(mo)
  def buchberger2(fs: Seq[QPoly])(implicit mo: MonomialOrder): Seq[QPoly] =
    Poly.buchberger2[BigInt,Q,QTerm,QPoly](fs)(mo)
  def buchberger3(fs: Seq[QPoly])(implicit mo: MonomialOrder): Seq[QPoly] =
    Poly.buchberger3[BigInt,Q,QTerm,QPoly](fs)(mo)
  def reduce(fs: Seq[QPoly])(implicit mo: MonomialOrder): Seq[QPoly] =
    Poly.reduce[BigInt,Q,QTerm,QPoly](fs)(mo)
  def string2poly(s: String)(implicit monoOrd: MonomialOrder): QPoly = {
    def f(revtokens: List[String], poly: QPoly): QPoly = revtokens match {
      case tstring :: Nil => poly + QPoly(QTerm.string2term(tstring)::Nil)
      case tstring :: "+" :: rest => f(rest, poly + QTerm.string2term(tstring))  
      case tstring :: "-" :: rest => f(rest, poly - QTerm.string2term(tstring))  
      case _ => throw new NumberFormatException(s)
    }
    f(s.split(" ").reverse.toList, QPoly(Nil))
  }
}
