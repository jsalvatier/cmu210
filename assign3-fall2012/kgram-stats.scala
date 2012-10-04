import scala.collection.parallel.immutable._

object kgramstats {
  def histogram[A <% Ordered[A]] (a : ParSeq[A]) : ParSeq[(A, Int)] = {

    def histmerge(a : ParSeq[(A, Int)], b : ParSeq[(A, Int)]) : ParSeq[(A, Int)] = { 
      (a, b) match { 
        case (a, ParSeq()) => a 
        case (ParSeq(), b) => b
        case (ParSeq((va, ca), as@_*), ParSeq((vb, cb), bs@_*)) => 
          (va compare vb) match { 
            case 0  => histmerge(as, bs) :+ (va, ca + cb)
            case -1 => histmerge(as, b) :+ (va, ca)
            case 1  => histmerge(as, b) :+ (vb, cb)   
            }
      }

    val b =a.map(v => ParSeq((v,1)))
    b.reduce(histmerge)
  }

}
