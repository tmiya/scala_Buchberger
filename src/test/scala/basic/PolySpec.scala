package basic

import org.specs2.mutable._

class PolySpec extends Specification {
  "with lex order" should {
    implicit val mo = LexOrder(List('x, 'y, 'z))
    "ts" in {
      QPoly(Seq(
        QTerm(Mono(Seq(1,2,3)), Q(0,1)),
        QTerm(Mono(Seq(1,3,3)), Q(1,1)),
        QTerm(Mono(Seq(2,3,3)), Q(2,1))
      )) === "2x^2y^3z^3 + xy^3z^3".poly
    }
    "LC" in {
      QPoly(Seq(
        QTerm(Mono(Seq(1,2,3)), Q(0,1)),
        QTerm(Mono(Seq(1,3,3)), Q(1,1)),
        QTerm(Mono(Seq(2,3,3)), Q(2,1))
      )).LC === Q(2,1)
    }
    "LT" in {
      QPoly(Seq(
        QTerm(Mono(Seq(1,2,3)), Q(0,1)),
        QTerm(Mono(Seq(1,3,3)), Q(1,1)),
        QTerm(Mono(Seq(2,3,3)), Q(2,1))
      )).LT === "2x^2y^3z^3".term
    }
    "LM" in {
      QPoly(Seq(
        QTerm(Mono(Seq(1,2,3)), Q(0,1)),
        QTerm(Mono(Seq(1,3,3)), Q(1,1)),
        QTerm(Mono(Seq(2,3,3)), Q(2,1))
      )).LM === "x^2y^3z^3".mono
    }
    "isZero" in {
      QPoly(Seq(
        QTerm(Mono(Seq(1,2,3)), Q(0,1)),
        QTerm(Mono(Seq(1,3,3)), Q(1,1)),
        QTerm(Mono(Seq(2,3,3)), Q(2,1))
      )).isZero === false
      QPoly(Seq(
        QTerm(Mono(Seq(1,2,3)), Q(0,1)),
        QTerm(Mono(Seq(1,3,3)), Q(0,1)),
        QTerm(Mono(Seq(2,3,3)), Q(0,1))
      )).isZero === true
    }
    "+" in {
      "2x^2y^3z^3 + xy^3z^3".poly + ("xy^3z^3".mono, Q(1,2)) === 
        "2x^2y^3z^3 + 3/2xy^3z^3".poly
      "2x^2y^3z^3 + xy^3z^3".poly + "1/2xy^3z^3".term === 
        "2x^2y^3z^3 + 3/2xy^3z^3".poly
      "2x^2y^3z^3 + xy^3z^3".poly + "2x^2y^3z^3 + xy^3z^4".poly === 
        "4x^2y^3z^3 + xy^3z^3 + xy^3z^4".poly
    }
    "-" in {
      "2x^2y^3z^3 + xy^3z^3".poly - ("xy^3z^3".mono, Q(1,2)) === 
        "2x^2y^3z^3 + 1/2xy^3z^3".poly
      "2x^2y^3z^3 + xy^3z^3".poly - "1/2xy^3z^3".term === 
        "2x^2y^3z^3 + 1/2xy^3z^3".poly
      "2x^2y^3z^3 + xy^3z^3".poly - "2x^2y^3z^3 + xy^3z^4".poly === 
        "xy^3z^3 - xy^3z^4".poly
    }
    "*" in {
      "2x^2y^3z^3 + xy^3z^3".poly * Q(1,2) === 
        "x^2y^3z^3 + 1/2xy^3z^3".poly
      "2x^2y^3z^3 + xy^3z^3".poly * ("xyz".mono, Q(1,2)) === 
        "x^3y^4z^4 + 1/2x^2y^4z^4".poly
      "2x^2y^3z^3 + xy^3z^3".poly * "1/2xyz".term === 
        "x^3y^4z^4 + 1/2x^2y^4z^4".poly
      "2x^2y^3z^3 + xy^3z^3".poly * "x - y".poly === 
        "2x^3y^3z^3 + x^2y^3z^3 - 2x^2y^4z^3 - xy^4z^3".poly
    }
    "/" in {
      "2x^2y^3z^3 + xy^3z^3".poly / Q(1,2) === 
        "4x^2y^3z^3 + 2xy^3z^3".poly
      "2x^2y^3z^3 + xy^3z^3".poly / ("xyz".mono, Q(1,2)) === 
        "4xy^2z^2 + 2y^2z^2".poly
      "2x^2y^3z^3 + xy^3z^3".poly / "1/2xyz".term === 
        "4xy^2z^2 + 2y^2z^2".poly
    }
    "divmod" in {
      "xy^2 + 1".poly.divmod(Seq("xy + 1".poly, "y + 1".poly)) ===
        (Seq("y".poly, "-1".poly), "2".poly)
      "x^2y + xy^2 + y^2".poly.divmod(Seq("xy - 1".poly, "y^2 - 1".poly)) ===
        (Seq("x + y".poly, "1".poly), "x + y + 1".poly)
    }
  }
  "with grlex order" should {
    implicit val mo = GrLexOrder(List('x, 'y, 'z))
    "S" in {
      QPoly.S("x^3y^2 - x^2y^3 + x".poly, "3x^4y + y^2".poly) ===
        "-x^3y^3 + x^2 - 1/3y^3".poly
    }
    "buchberger" in {
      QPoly.buchberger(Seq("x^3 - 2xy".poly, "x^2y - 2y^2 + x".poly)) ===
        Seq("x^3 - 2xy".poly, "x^2y - 2y^2 + x".poly, "-x^2".poly,
            "-2xy".poly, "-2y^2 + x".poly)
    }
    "buchberger3/reduce" in {
      QPoly.reduce(QPoly.buchberger(Seq("x^3 - 2xy".poly, "x^2y - 2y^2 + x".poly))).toSet ===
        Set("x^2".poly, "xy".poly, "y^2 - 1/2x".poly)
    }
  }
}