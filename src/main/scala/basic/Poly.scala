package basic

case class Poly(cmonos: Seq[(Mono,Q)])(implicit mo: MonomialOrder) {
  val cms: Seq[(Mono,Q)] = cmonos.filter{case (_,c) => c.n != 0}.sortWith{case (cx, cy) => cy._1 < cx._1}
  override def toString: String = 
    if (cms.length == 0) "0" 
    else cms.map{case (m,c) => {
      val (cs, ms) = (c.toString, m.toString)
      if (ms == "1") cs
      else if (cs == "1") ms
      else if (cs == "-1") "-"+ms
      else cs+ms
    }}.mkString(" + ")
  override def equals(a:Any): Boolean = a.isInstanceOf[Poly] && (cms == a.asInstanceOf[Poly].cms)
  def LT: (Mono,Q) = cms.head
  def LC: Q = cms.head._2
  def LM: Mono = cms.head._1
  def + (rhs: Poly): Poly = {
    val map: scala.collection.mutable.Map[Mono,Q] = scala.collection.mutable.Map(cms:_*)
    rhs.cms.foreach{case (m,c) => map.get(m) match {
      case None => map.put(m, c)
      case Some(q) => {var s = q+c; if (s.n==0) map.remove(m) else map(m) = s}
    }}
    Poly(map.toSeq)
  }
  def + (m: Mono, c:Q): Poly = {
    val map: scala.collection.mutable.Map[Mono,Q] = scala.collection.mutable.Map(cms:_*)
    map.get(m) match {
      case None => map.put(m,c)
      case Some(q) => {var s = q + c; if (s.n==0) map.remove(m) else map(m) = s}
    }
    Poly(map.toSeq)
  }
  def + (t: (Mono,Q)): Poly = this + (t._1, t._2)
  def - (t: (Mono,Q)): Poly = this + (t._1, -t._2)
  def - (rhs: Poly): Poly = {
    val map: scala.collection.mutable.Map[Mono,Q] = scala.collection.mutable.Map(cms:_*)
    rhs.cms.foreach{case (m,c) => map.get(m) match {
      case None => map.put(m, -c)
      case Some(q) => {var s = q - c; if (s.n==0) map.remove(m) else map(m) = s}
    }}
    Poly(map.toSeq)
  }
  def * (mono:Mono, q:Q): Poly = 
    if (q.n==0) Poly(Nil) else Poly(cms.map{case (m,c) => (m*mono,c*q)})
  def * (rhs: Poly): Poly = {
    val map: scala.collection.mutable.Map[Mono,Q] = scala.collection.mutable.Map.empty
    for((m1,c1) <- cms; (m2,c2) <- rhs.cms)
    yield {
      val m = m1*m2
      map.get(m) match {
        case None => map.put(m, c1*c2)
        case Some(q) => {var s = q+c1*c2; if (s.n==0) map.remove(m) else map(m) = s}
      }
    }
    Poly(map.toSeq)
  }
  def / (mono:Mono=Mono(mo.vars.map{s => 0}), q:Q=Q(1,1)): Poly = 
    if (q.n==0) Poly(Nil) else Poly(cms.map{case (m,c) => (m/mono,c/q)})
  def divmod(fs: Seq[Poly]):(Seq[Poly], Poly) = {
    //println("divmod")
    if (cms.isEmpty) (fs.map{f => Poly(Nil)}, Poly(Nil)) 
    else {
      val s:Int = fs.length
      var r:Poly = Poly(Nil)
      val a:Array[Poly] = fs.view.map{f => Poly(Nil)}.toArray
      var p:Poly = this
      while(p != Poly(Nil)) {
        var i = 0
        var div_occur = false
        while(i < s && !div_occur) {
          if (fs(i).LM | p.LM) {
            val m = p.LM / fs(i).LM
            val c = p.LC / fs(i).LC
            a(i) = a(i) + (m,c)
            p = p - fs(i) * (m,c)
            div_occur = true
          } else {i += 1}
        }
        if (!div_occur) {
          r = r + p.LT
          p = p - (p.LM, p.LC)
        }
      }
      (a.toSeq, r)
    }
  }
  def bar(fs: Seq[Poly]): Poly = divmod(fs)._2
}
object Poly {
  def S(f:Poly, g:Poly)(implicit mo: MonomialOrder): Poly = {
    val xg = Mono.LCM(f.LM, g.LM)
    f * (xg / f.LM, f.LC.inv) - g * (xg / g.LM, g.LC.inv)
  }
  def buchberger(fs: Seq[Poly])(implicit mo: MonomialOrder): Seq[Poly] = {
    var gs: scala.collection.mutable.ArrayBuffer[Poly] = scala.collection.mutable.ArrayBuffer[Poly](fs:_*)
    var cont = true
    while(cont) {
      val gss = scala.collection.mutable.ArrayBuffer[Poly](gs:_*)
      val nss = gss.length
      var found = false
      var i = 0
      while(!found && i<nss) {
        val p = gss(i)
        var j = i+1
        while(!found && j<nss) {
          val q = gss(j)
          val s = Poly.S(p,q).bar(gss)
          if (s != Poly(Nil)) {gs += s; found = true}
          j += 1
        }
        i += 1
      }
      if (gs.length == gss.length) {cont = false}
    }
    gs.toSeq
  }
  def reduce(fs: Seq[Poly])(implicit mo: MonomialOrder): Seq[Poly] = {
    def red(ps: List[Poly], acc:List[Poly]): List[Poly] = ps match {
      case Nil => acc
      case p::pss => if (pss.exists{pi => pi.LM | p.LM} || acc.exists{pi => pi.LM | p.LM}) red(pss,acc) else red(pss, p::acc)
    }
    red(fs.toList, Nil).reverse.map{p => p / (Mono(mo.vars.map{_ => 0})(mo), p.LC)}
  }
  def buchberger2(fs: Seq[Poly])(implicit mo: MonomialOrder): Seq[Poly] = {
    val s: Int = fs.length
    val b:scala.collection.mutable.Set[(Int,Int)] = scala.collection.mutable.Set.empty
    for(i <- Range(0,s); j <- Range(i+1, s)) yield {b += ((i,j))}
    val gs: scala.collection.mutable.ArrayBuffer[Poly] = scala.collection.mutable.ArrayBuffer[Poly](fs:_*)
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
        val spair = Poly.S(fi, fj).bar(gs)
        if (spair != Poly(Nil)) {
          t = t+1
          gs += spair
          for(ii <- Range(0, t-1)) yield {b += ((ii,t-1))}
        }
      }
      b -= ((i,j))
    }    
    gs.toSeq
  }
  def buchberger3(fs: Seq[Poly])(implicit mo: MonomialOrder): Seq[Poly] = {
    val s: Int = fs.length
    val b:scala.collection.mutable.Set[(Int,Int)] = scala.collection.mutable.Set.empty
    for(i <- Range(0,s); j <- Range(i+1, s)) yield {b += ((i,j))}
    val gs: scala.collection.mutable.ArrayBuffer[Poly] = scala.collection.mutable.ArrayBuffer[Poly](fs:_*)
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
        val spair = Poly.S(fi, fj).bar(gs.sortBy{g => g.LM.deg})
        if (spair != Poly(Nil)) {
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
