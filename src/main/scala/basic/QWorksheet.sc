package basic

object QWorksheet {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  // Q
  Q(1, 2) + Q(1, 3)                               //> res0: basic.Q = 5/6
  Q(1, 2) - Q(1, 3)                               //> res1: basic.Q = 1/6
  Q(2, 2)                                         //> res2: basic.Q = 1
  1.q + Q(1, 2)                                   //> res3: basic.Q = 3/2
  Q(12, 34) + Q(56, 78)                           //> res4: basic.Q = 710/663
  Q(12, 34) - Q(56, 78)                           //> res5: basic.Q = -242/663
  Q(12, 34) * Q(56, 78)                           //> res6: basic.Q = 56/221
  Q(12, 34) / Q(56, 78)                           //> res7: basic.Q = 117/238
  "-123/456".q                                    //> res8: basic.Q = -41/152

  // lex
  val lex = LexOrder(List('x,'y,'z))              //> lex  : basic.LexOrder = LexOrder(List('x, 'y, 'z))
  // grlex
  val grlex = GrLexOrder(List('x,'y,'z))          //> grlex  : basic.GrLexOrder = GrLexOrder(List('x, 'y, 'z))
  Mono(Seq(1,2,3))(grlex) > Mono(Seq(3,2,0))(grlex)
                                                  //> res9: Boolean = true
  Mono(Seq(1,2,4))(grlex) > Mono(Seq(1,1,5))(grlex)
                                                  //> res10: Boolean = true
  // grevlex
  val grevlex = GRevLexOrder(List('x,'y,'z))      //> grevlex  : basic.GRevLexOrder = GRevLexOrder(List('x, 'y, 'z))

  {implicit val mo = lex; Mono(Seq(1,2,0)) > Mono(Seq(0,3,4))}
                                                  //> res11: Boolean = true
  {implicit val mo = lex; Mono(Seq(3,2,4)) > Mono(Seq(3,2,1))}
                                                  //> res12: Boolean = true
  {implicit val mo = lex; "xy^2 + 1".poly.divmod(Seq("xy + 1".poly, "y + 1".poly))}
                                                  //> res13: (Seq[basic.QPoly], basic.QPoly) = (WrappedArray(y, -1),2)
  {implicit val mo = grlex; Mono(Seq(1,2,3)) > Mono(Seq(3,2,0))}
                                                  //> res14: Boolean = true
  {implicit val mo = grlex; Mono(Seq(1,2,4)) > Mono(Seq(1,1,5))}
                                                  //> res15: Boolean = true
  {implicit val mo = grlex; "xy^2 + 1".poly.divmod(Seq("xy + 1".poly, "y + 1".poly))}
                                                  //> res16: (Seq[basic.QPoly], basic.QPoly) = (WrappedArray(y, -1),2)

  {implicit val mo = grevlex; Mono(Seq(4,7,1)) > Mono(Seq(4,2,3))}
                                                  //> res17: Boolean = true
  {implicit val mo = grevlex; Mono(Seq(1,5,2)) > Mono(Seq(4,1,3))}
                                                  //> res18: Boolean = true
  {implicit val mo = grevlex; "xy^2 + 1".poly.divmod(Seq("xy + 1".poly, "y + 1".poly))}
                                                  //> res19: (Seq[basic.QPoly], basic.QPoly) = (WrappedArray(y, -1),2)

  {implicit val mo = grlex;
   QPoly.S("x^3y^2 - x^2y^3 + 1".poly, "3x^4y + y^2".poly)
  }                                               //> res20: basic.QPoly = -x^3y^3 + -1/3y^3 + x
  {implicit val mo = grlex;
   QPoly.buchberger(Seq("x^3 - 2xy".poly, "x^2y - 2y^2 + x".poly))
  }                                               //> res21: Seq[basic.QPoly] = ArrayBuffer(x^3 + -2xy, x^2y + -2y^2 + x, -x^2, -
                                                  //| 2xy, -2y^2 + x)
  {implicit val mo = grlex;
   QPoly.reduce(QPoly.buchberger(Seq("x^3 - 2xy".poly, "x^2y - 2y^2 + x".poly)))
  }                                               //> res22: Seq[basic.QPoly] = List(x^2, xy, y^2 + -1/2x)
  // Exercise 13
  {implicit val mo = grevlex
   QPoly.reduce(QPoly.buchberger3(Seq(
     "x^5 + y^4 + z^3 - 1".poly, "x^3 + y^2 + z^2 - 1".poly
   )))
  }                                               //> res23: Seq[basic.QPoly] = List(x^3 + z^2 + y^2 + -1, y^4 + -x^2z^2 + -x^2y^
                                                  //| 2 + z^3 + x^2 + -1)
  /*
  {implicit val mo = lex
   QPoly.reduce(QPoly.buchberger3(Seq(
     "x^5 + y^4 + z^3 - 1".poly, "x^3 + y^2 + z^2 - 1".poly
   )))
  }
  {implicit val mo = LexOrder(Seq('a, 'b, 'c, 'd, 'e))
   QPoly.reduce(QPoly.buchberger3(Seq(
     "a + b + c + d + e".poly,
     "ab + bc + cd + de + ea".poly,
     "abc + bcd + cde + dea + eab".poly,
     "abcd + bcde + cdea + deab + eabc".poly,
     "abcde - 1".poly
   )))
  }
   */
}