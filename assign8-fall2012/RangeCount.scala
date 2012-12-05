import Range_Count._
import BSTOrderedTable._
import Ordering.Implicits._
import math.max

object BSTRangeCount extends Range_Count { 

  val table = BSTOrderedTable.BSTOrderedTable[Int](implicitly[Ordering[Int]])
  import table._

  type CountTable = Table[Table[Unit]]
  
  def makeQueryTable(ps : Seq[Point]) : CountTable = { 
    val pss = ps.sorted(Ordering.by[Point,Int](_.x))
    pss.foldLeft((empty : CountTable, empty : Table[Unit]))( sweepline)._1 
  }

  /*task 3.2 

  in order to parallelize makeQueryTable. I would implement treating OrderedTable like a seq and then do a scan operation (which runs in
    parallel). i would use scan to replace the foldLeft. sweepline would have to be an associative operator which unions two orderedsets
  (ordered in the y direction). 

  */

 /*
 task 3.3

 the first level tree should use n nodes at max
 each second level tree should use n nodes but differ from the tree before it in the sequence by only log n nodes. 
 summing, we get n * log n nodes total 
 so in total we have n + n log n = n log n nodes
  */

  def sweepline(ts : (CountTable, Table[Unit]), p : Point) : (CountTable, Table[Unit]) = {
    val (xt, yt) = ts
    val nyt = insert(yt) (p.y, Unit)
    val nxt = insert(xt) (p.x, nyt)
    (nxt, nyt)
  }

  def countInRange( t : CountTable)(p1 : Point, p2 : Point) : Int = { 
    val xrng = getRange(t, p1.x, p2.x)
    def sz(t : Option[(Int, Table[Unit])]) : Int =size(getRange((t map (_._2)) getOrElse empty, p1.y, p2.y ))
    
    sz(last(xrng)) - max(sz(first(xrng)) - 1, 0)
  }
}
