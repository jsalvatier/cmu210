import scala.math 
import scala.collection.parallel.immutable

object stock { 
  def maxj(a : Vector[Int]) : (Int, Int, Int, Int) = {
      if (a.length > 1){
        val (split1, split2) = a.splitAt(a.length/2)
        val (lpremax, lmax, lpostmax, ltot) = maxj(split1)
        val (rpremax, rmax, rpostmax, rtot) = maxj(split2) 
        val r = (math.max(lpremax, ltot + rpremax),
                 math.max( math.max(lmax, rmax), lpostmax+rpremax),
                 math.max(rpostmax, rtot + lpostmax),
                 ltot + rtot)
        r
      }
      else {
          val v = math.max(0, a(0)) 
          (v,v,v, a(0))
      }
    }

  def maxjump(a : Vector[Int]) : Int = maxj(a)._2
}

sealed abstract class Paren
case object OParen extends Paren
case object CParen extends Paren

object parens {
  
  def count(v :  Vector[Paren]) : (Int, Int)  = {
    if (v.length > 1) {
      val (lv, rv) = v.splitAt(v.length/2)
      val (ll, lr) = count(lv)
      val (rl, rr) = count(rv)
      val d = rl - lr
      if (d > 0) 
        (ll + d, rr)
      else 
        (ll, rr - d) 
    }
    else
      v(0) match {
        case OParen => (0, 1)
        case CParen => (1, 0)
      }
  
  }
  // count parens in parallel
  def pcount ( v : Vector[Paren]) : (Int, Int) = v.par.map(toCount).reduce(countMerge)  

  def countMerge ( l : (Int, Int), r : (Int, Int)) : (Int, Int) = {
    val (ll, lr) = l 
    val (rl, rr) = r 
    val d = rl - lr 
    if (d > 0)
      (ll + d, rr)
    else 
      (ll, rr - d)
  }
  def toCount(p : Paren) = 
    p match {
        case OParen => (0, 1)
        case CParen => (1, 0)
      }
}
