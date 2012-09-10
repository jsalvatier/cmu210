import scala.math 
import scala.collection.parallel.immutable

object stock{ 

  def maxjump(a : ParVector[Int]) : Int = {
        
    def maxj(b : Int, e : Int) : (Int, Int, Int, Int) = {
      val len = e - b
      if (len > 1){
        val split = len/2 + b 
        val (lpremax, lmax, lpostmax, ltot) = maxj(b, split)
        val (rpremax, rmax, rpostmax, rtot) = maxj(split, e) 
        val a = (math.max(lpremax, ltot + rpremax),
                 math.max( math.max(lmax, rmax), lpostmax+rpremax),
                 math.max(rpostmax, rtot + lpostmax),
                 ltot + rtot)
        println(a)
        a
      }
      else {
        if (len == 0){
          println(0)
          (0,0,0,0) 
        }
        else { 
          val v = math.max(0, a(b)) 
          println (v)
          (v,v,v, a(b))
        }
      }
    }
    maxj(0, a.length)._2
  }

  def maxjump2(a : Array[Int]) : Int = {
    def ext( c : (Int, Int, Int), v : Int) : (Int , Int, Int) = {
      (max
    }
    a.foldLeft((0,0,0,0))(ext)
}
