package basic

import org.specs2.mutable._

class TermSpec extends Specification {
  "with lex order" should {
    implicit val mo = LexOrder(List('x, 'y, 'z))
    "isZero" in {
      QTerm(Mono(Seq(1,2,3)), Q(0,1)).isZero === true
      QTerm(Mono(Seq(1,2,3)), Q(3,1)).isZero === false
    }
    "|" in {
      ("1/2xy^2".term | "xy^3z^4".term) === true
    }
    "*" in {
      "1/2xy^2".term * "xy^3z^4".term === "1/2x^2y^5z^4".term
    }
    "/" in {
      "xy^3z^4".term / "1/2xy^2".term === "2yz^4".term
    }
  }
}