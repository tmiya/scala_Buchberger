package basic

import org.specs2.mutable._

class LexOrderSpec extends Specification {
  implicit val mo = LexOrder(List('x,'y,'z))  
  "lex order" should {
    "xy^2 > y^3z^4" in {
      ("xy^2".mono > "y^3z^4".mono) === true
    }
    "x^3y^2z^4 > x^3y^2z" in {
      ("x^3y^2z^4".mono > "x^3y^2z".mono) === true
    }
  }
}
class GrLexOrderSpec extends Specification {
  implicit val mo = GrLexOrder(List('x,'y,'z))  
  "grlex order" should {
    "xy^2z^3 > x^3y^2" in {
      ("xy^2z^3".mono > "x^3y^2".mono) === true
    }
    "xy^2z^4 > xyz^5" in {
      ("xy^2z^4".mono > "xyz^5".mono) === true
    }
  }
}
class GRevLexOrderSpec extends Specification {
  implicit val mo = GRevLexOrder(List('x,'y,'z))  
  "grevlex order" should {
    "x^4y^7z > x^4y^2z^3" in {
      ("x^4y^7z".mono > "x^4y^2z^3".mono) === true
    }
    "xy^5z^2 > x^4yz^3" in {
      ("xy^5z^2".mono > "x^4yz^3".mono) === true
    }
  }
}