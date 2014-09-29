package basic

import scala.math.Ordering
import scala.util.matching.Regex

/**
 * trait for monomial order
 */
trait MonomialOrder extends Ordering[Mono]{
  /** list of variables */
  val vars: Seq[Symbol]
  /** comparison between Mono-s */
  def compare(x: Mono, y: Mono): Int
}

/** lexicographic order */
case class LexOrder(val vars: Seq[Symbol]) extends MonomialOrder {
  def compare(x: Mono, y: Mono): Int = compare(x.degs, y.degs)
  private def compare(xs: Seq[Int], ys: Seq[Int]): Int = {
    if (xs.isEmpty || ys.isEmpty) 0
    else {
      val c = xs.head compare ys.head
      if (c!=0) c else compare(xs.tail, ys.tail)
    }    
  }
}

/** graded lex order */
case class GrLexOrder(val vars: Seq[Symbol]) extends MonomialOrder {
  private val lexOrder = LexOrder(vars)
  def compare(x: Mono, y: Mono): Int = {
    val c = x.deg compare y.deg
    c match {
      case 0 => lexOrder.compare(x, y)
      case _ => c
    }
  }
}

/** graded reverse lex order */
case class GRevLexOrder(val vars: Seq[Symbol]) extends MonomialOrder {
  private val lexOrder = LexOrder(vars.reverse)
  def compare(x: Mono, y: Mono): Int = {
    val c = x.deg compare y.deg
    c match {
      case 0 => -lexOrder.compare(x, y)
      case _ => c
    }
  }
}
