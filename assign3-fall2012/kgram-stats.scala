import scala.collection.parallel.immutable._

class kgramstats[Token <% Ordered[A]]{
  type Token = String
  type Hist[A] = ParMap[A, Int]
  type KGram = Seq[Token]

  
  def histmerge(a : Hist, b : Hist) : Hist = unionWith(_ + _, a, b) 
  
  def unionWith[A,B](f : A => A => A)(a : ParMap[A, B], b : ParMap[A, B]) : ParMap[A, B] = 
    (a -- b.keySet) ++ b.map {
      case (k, v) => k -> (a get k map { f(k, v, _) } getOrElse v)
      }
 
  def histogram(a : ParSeq[Token]) : Hist = 
    a.map(v => (empty + (v, 1))).reduce(histmerge)

  type KGramStats = Hist[KGram]

  case class Summary(pre: KGram, tot : Option[KGram],post : Token, stats : KGramStats) 

  def concat(k : Int)( a : Summary, b : Summary) : Summary) = { 
    val pre = a.tot match {
      None => a.pre
      Some(t) => t :+ b.pre.take(k - t.length) 
    }
    val tot = (a.tot, b.tot) match { 
      (Some(t1), Some(t2)) => if (t1.length + t2.length <= k) 
                                Some(t1 :+ t2)
                              else 
                                None
      (_,_) => None
    }
    Summary(pre, tot, b.post, merge(a.stats, merge(single pre, b.stats)))
  }

  def single(v : KGram) : KGramStats = ParMap() + (v, 1)
  def toSum(t : Token) : Summary = Summary( ParSeq(t),ParSeq(t), t, single t)
  def make_stats(s : ParSeq[Token], k : Int) : KGramStats = 
    s.map(toSum).reduce(concat(k))

  def lookup_freq(s : KGramStats, k : KGram, t : Token) : (Int, Int) = 

}

