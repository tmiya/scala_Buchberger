package object basic {
  /** Int to Q conversion. 1.q -> Q(1,1) */
  implicit class int2Q(val i:Int) extends AnyVal {
    def q: Q = Q(BigInt(i), BigInt(1))
  }
  /** String to Q conversion. "-123/456".q -> Q(123,456) */
  implicit class QHelper(val s: String) {
    def q: Q = Q.string2Q(s)
  }
  /** String to Q conversion. "-123/456".q -> Q(123,456) */
  implicit class StringHelper(val s: String)(implicit mo: MonomialOrder) {
    def mono: Mono = Mono.string2mono(s)(mo)
    def term: QTerm = QTerm.string2term(s)(mo)
    def poly: QPoly = QPoly.string2poly(s)(mo)
  }
}