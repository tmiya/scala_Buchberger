package basic

object QWorksheet {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  // Q
  Q(1, 2) + Q(1, 3)                               //> res0: basic.Q = 5/6
  Q(1, 2) - Q(1, 3)                               //> res1: basic.Q = 1/6
  Q(2, 2)                                         //> res2: basic.Q = 1
  1 + Q(1, 2)                                     //> res3: basic.Q = 3/2
  // lex
  val lex = LexOrder(List('x,'y,'z))              //> lex  : basic.LexOrder = LexOrder(List('x, 'y, 'z))
  Mono(Seq(1, 2, 0))(lex)                         //> res4: basic.Mono = xy^2
  Mono(Seq(1,2,0))(lex) > Mono(Seq(0,3,4))(lex)   //> res5: Boolean = true
  Mono(Seq(3,2,4))(lex) > Mono(Seq(3,2,1))(lex)   //> res6: Boolean = true
  // grlex
  val grlex = GrLexOrder(List('x,'y,'z))          //> grlex  : basic.GrLexOrder = GrLexOrder(List('x, 'y, 'z))
  Mono(Seq(1,2,3))(grlex) > Mono(Seq(3,2,0))(grlex)
                                                  //> res7: Boolean = true
  Mono(Seq(1,2,4))(grlex) > Mono(Seq(1,1,5))(grlex)
                                                  //> res8: Boolean = true
  // grevlex
  val grevlex = GRevLexOrder(List('x,'y,'z))      //> grevlex  : basic.GRevLexOrder = GRevLexOrder(List('x, 'y, 'z))
  
  Mono(Seq(4,7,1))(grevlex) > Mono(Seq(4,2,3))(grevlex)
                                                  //> res9: Boolean = true
  Mono(Seq(1,5,2))(grevlex) > Mono(Seq(4,1,3))(grevlex)
                                                  //> res10: Boolean = true
  val f0_in_rex = Poly(Seq(
    (Mono(Seq(3,0,0))(lex), Q(-5,1)),
    (Mono(Seq(2,0,2))(lex), Q(7,1)),
    (Mono(Seq(1,2,1))(lex), Q(4,1)),
    (Mono(Seq(0,0,2))(lex), Q(4,1))
  ))(lex)                                         //> f0_in_rex  : basic.Poly = -5x^3 + 7x^2z^2 + 4xy^2z + 4z^2
  f0_in_rex.LT                                    //> res11: (basic.Mono, basic.Q) = (x^3,-5)
  f0_in_rex.LM                                    //> res12: basic.Mono = x^3
  f0_in_rex.LC                                    //> res13: basic.Q = -5
  Poly(Seq(
    (Mono(Seq(3,0,0))(grlex), Q(-5,1)),
    (Mono(Seq(2,0,2))(grlex), Q(7,1)),
    (Mono(Seq(1,2,1))(grlex), Q(4,1)),
    (Mono(Seq(0,0,2))(grlex), Q(4,1))
  ))(grlex)                                       //> res14: basic.Poly = 7x^2z^2 + 4xy^2z + -5x^3 + 4z^2
  Poly(Seq(
    (Mono(Seq(3,0,0))(grevlex), Q(-5,1)),
    (Mono(Seq(2,0,2))(grevlex), Q(7,1)),
    (Mono(Seq(1,2,1))(grevlex), Q(4,1)),
    (Mono(Seq(0,0,2))(grevlex), Q(4,1))
  ))(grevlex)                                     //> res15: basic.Poly = 4xy^2z + 7x^2z^2 + -5x^3 + 4z^2
  // divmod
  val div_f =
  Poly(Seq(
    (Mono(Seq(1,2,0))(lex), Q(1,1)),
    (Mono(Seq(0,0,0))(lex), Q(1,1))
  ))(lex)                                         //> div_f  : basic.Poly = xy^2 + 1
  val div_f1 = Poly(Seq(
      (Mono(Seq(1,1,0))(lex), Q(1,1)),
      (Mono(Seq(0,0,0))(lex), Q(1,1))
    ))(lex)                                       //> div_f1  : basic.Poly = xy + 1
  val div_f2 = Poly(Seq(
      (Mono(Seq(0,1,0))(lex), Q(1,1)),
      (Mono(Seq(0,0,0))(lex), Q(1,1))
    ))(lex)                                       //> div_f2  : basic.Poly = y + 1
  div_f.divmod(Seq(div_f1, div_f2))               //> res16: (Seq[basic.Poly], basic.Poly) = (WrappedArray(y, -1),2)
  
  
  val s_f = Poly(Seq(
    (Mono(Seq(3,2,0))(grlex),Q(1,1)),
    (Mono(Seq(2,3,0))(grlex),Q(-1,1)),
    (Mono(Seq(1,0,0))(grlex),Q(1,1))
    ))(grlex)                                     //> s_f  : basic.Poly = x^3y^2 + -x^2y^3 + x
   val s_g = Poly(Seq(
    (Mono(Seq(4,1,0))(grlex),Q(3,1)),
    (Mono(Seq(0,2,0))(grlex),Q(1,1))
    ))(grlex)                                     //> s_g  : basic.Poly = 3x^4y + y^2
   Poly.S(s_f, s_g)(grlex)                        //> res17: basic.Poly = -x^3y^3 + -1/3y^3 + x^2
   // buchberger
  val buch_f1 = Poly(Seq(
     (Mono(Seq(3,0,0))(grlex), Q(1,1)),
     (Mono(Seq(1,1,0))(grlex), Q(-2,1))
     ))(grlex)                                    //> buch_f1  : basic.Poly = x^3 + -2xy
  val buch_f2 = Poly(Seq(
     (Mono(Seq(2,1,0))(grlex), Q(1,1)),
     (Mono(Seq(0,2,0))(grlex), Q(-2,1)),
     (Mono(Seq(1,0,0))(grlex), Q(1,1))
     ))(grlex)                                    //> buch_f2  : basic.Poly = x^2y + -2y^2 + x
  val buch_base1 = Poly.buchberger(Seq(buch_f1, buch_f2))(grlex)
                                                  //> buch_base1  : Seq[basic.Poly] = ArrayBuffer(x^3 + -2xy, x^2y + -2y^2 + x, -
                                                  //| x^2, -2xy, -2y^2 + x)
  
  
  val buch_base2 = Poly.reduce(buch_base1)(grlex) //> buch_base2  : Seq[basic.Poly] = List(x^2, xy, y^2 + -1/2x)
  
  val buch2_base1 = Poly.buchberger2(Seq(buch_f1, buch_f2))(grlex)
                                                  //> buch2_base1  : Seq[basic.Poly] = ArrayBuffer(x^3 + -2xy, x^2y + -2y^2 + x, 
                                                  //| -x^2, -2y^2 + x, -2xy)
  val buch3_f1 = Poly(Seq(
     (Mono(Seq(3,0,0))(lex), Q(1,1)),
     (Mono(Seq(1,1,0))(lex), Q(-2,1))
     ))(lex)                                      //> buch3_f1  : basic.Poly = x^3 + -2xy
  val buch3_f2 = Poly(Seq(
     (Mono(Seq(2,1,0))(lex), Q(1,1)),
     (Mono(Seq(0,2,0))(lex), Q(-2,1)),
     (Mono(Seq(1,0,0))(lex), Q(1,1))
     ))(lex)                                      //> buch3_f2  : basic.Poly = x^2y + x + -2y^2
  val buch3_base1 = Poly.buchberger3(Seq(buch3_f1, buch3_f2))(lex)
                                                  //> buch3_base1  : Seq[basic.Poly] = ArrayBuffer(x^3 + -2xy, x^2y + x + -2y^2, 
                                                  //| -x^2, x + -2y^2, 4y^4, -4y^3)
  val buch3_base2 = Poly.buchberger(Seq(buch3_f1, buch3_f2))(lex)
                                                  //> buch3_base2  : Seq[basic.Poly] = ArrayBuffer(x^3 + -2xy, x^2y + x + -2y^2, 
                                                  //| -x^2, -2xy, x + -2y^2, 4y^3)
  val buch3_base3 = Poly.reduce(buch3_base2)(lex) //> buch3_base3  : Seq[basic.Poly] = List(x + -2y^2, y^3)
  // Exercise 13
  val f13_1_grevlex = Poly(Seq(
     (Mono(Seq(5,0,0))(grevlex), Q(1,1)),
     (Mono(Seq(0,4,0))(grevlex), Q(1,1)),
     (Mono(Seq(0,0,3))(grevlex), Q(1,1)),
     (Mono(Seq(0,0,0))(grevlex), Q(-1,1))
     ))(grevlex)                                  //> f13_1_grevlex  : basic.Poly = x^5 + y^4 + z^3 + -1
  val f13_2_grevlex = Poly(Seq(
     (Mono(Seq(3,0,0))(grevlex), Q(1,1)),
     (Mono(Seq(0,2,0))(grevlex), Q(1,1)),
     (Mono(Seq(0,0,2))(grevlex), Q(1,1)),
     (Mono(Seq(0,0,0))(grevlex), Q(-1,1))
     ))(grevlex)                                  //> f13_2_grevlex  : basic.Poly = x^3 + z^2 + y^2 + -1
  val buch3_base13_grevlex = Poly.reduce(Poly.buchberger(Seq(f13_1_grevlex, f13_2_grevlex))(grevlex))(grevlex)
                                                  //> buch3_base13_grevlex  : Seq[basic.Poly] = List(x^3 + z^2 + y^2 + -1, y^4 + 
                                                  //| -x^2z^2 + -x^2y^2 + z^3 + x^2 + -1)
  /*
  val f13_1_lex = Poly(Seq(
     (Mono(Seq(5,0,0))(lex), Q(1,1)),
     (Mono(Seq(0,4,0))(lex), Q(1,1)),
     (Mono(Seq(0,0,3))(lex), Q(1,1)),
     (Mono(Seq(0,0,0))(lex), Q(-1,1))
     ))(lex)
  val f13_2_lex = Poly(Seq(
     (Mono(Seq(3,0,0))(lex), Q(1,1)),
     (Mono(Seq(0,2,0))(lex), Q(1,1)),
     (Mono(Seq(0,0,2))(lex), Q(1,1)),
     (Mono(Seq(0,0,0))(lex), Q(-1,1))
     ))(lex)
  val buch3_base13_lex = Poly.reduce(Poly.buchberger(Seq(f13_1_lex, f13_2_lex))(lex))(lex)
  */
  
}