package basic

import org.specs2.mutable._

class QSpec extends Specification {
  "Q(-123,456)" should {
    "n" in {
      Q(-123,456).n === BigInt(-41) // -41/152
    }
    "d" in {
      Q(-123,456).d === BigInt(152) // -41/152
    }
    "unary_-" in {
      -Q(-123,456) === Q(123,456)
    }
    "inv" in {
      Q(-123,456).inv === Q(-456,123)
    }
    "isZero" in {
      Q(-123,456).isZero === false
    }
    "toString" in {
      Q(-123,456).toString === "-41/152"
    }
  }
  "Q(10, 20) op Q(10, 30)" should {
    "+" in {
      Q(10,20) + Q(10,30) === Q(5,6)
    }
    "-" in {
      Q(10,20) - Q(10,30) === Q(1,6)
    }
    "*" in {
      Q(10,20) * Q(10,30) === Q(1,6)
    }
    "/" in {
      Q(10,20) / Q(10,30) === Q(3,2)
    }
    "compare" in {
      Q(10,20).compare(Q(10,30)) === 1
    }
  }
  "Q(0,1)" should {
    "isZero" in {
      Q(0,1).isZero === true
    }
    "toString" in {
      Q(0,1).toString === "0"
    } 
  }
  "QUtil.string2Q" should {
    "-123/456" in {
      Q.string2Q("-123/456") === Q(-123,456)
    }
  }
}