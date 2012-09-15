import scala.math 
import scala.collection.parallel.immutable

object stock{ 
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
