package basic

import org.specs2.mutable._

class MonoSpec extends Specification {
  "with lex order" should {
    implicit val mo = LexOrder(List('x, 'y, 'z))
    "compare" in {
      Mono(Seq(1,2,0)) > Mono(Seq(0,3,4))
      Mono(Seq(3,2,4)) > Mono(Seq(3,2,1))
    }
    "deg" in {
      Mono(Seq(1,2,0)).deg === 3
      "x^3y^2z^4".mono.deg === 9
    }
    "toString" in {
      Mono(Seq(1,2,0)).toString === "xy^2"
      Mono(Seq(3,2,4)).toString === "x^3y^2z^4"     
    }
    "|" in {
      (Mono(Seq(1,2,0)) | Mono(Seq(1,3,4))) === true
      (Mono(Seq(3,2,4)) | Mono(Seq(3,2,1))) === false
    }
    "*" in {
      (Mono(Seq(1,2,0)) * Mono(Seq(1,3,4))) === Mono(Seq(2,5,4))
      (Mono(Seq(3,2,4)) * Mono(Seq(3,2,1))) === Mono(Seq(6,4,5))
    }
    "/" in {
      (Mono(Seq(1,3,4)) / Mono(Seq(1,2,0))) === Mono(Seq(0,1,4))
    }
    "LCM" in {
      Mono.LCM(Mono(Seq(1,2,0)), Mono(Seq(1,3,4))) === Mono(Seq(1,3,4))
      Mono.LCM(Mono(Seq(3,2,4)), Mono(Seq(3,2,1))) === Mono(Seq(3,2,4))
    }
    "string2mono" in {
      "xy^3z^4".mono === Mono(Seq(1,3,4))
      "x^3y^2z^4".mono === Mono(Seq(3,2,4))
    }
  }
}