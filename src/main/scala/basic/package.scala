package object basic {
  implicit class int2Q(val i:Int) extends AnyVal {
    def toQ: Q = Q(i, 1)
    def + (r:Q) :Q = Q(i, 1) + r
    def - (r:Q) :Q = Q(i, 1) - r
    def * (r:Q) :Q = Q(i, 1) * r
    def / (r:Q) :Q = Q(i, 1) / r
  }
}