import BSTOrderedTable._
import util._

object OrderedTableTest { 

  val t = BSTOrderedTable.BSTOrderedTable[Int](implicitly[Ordering[Int]])
  import t._

  def randSeq(s: Int, n : Int ) : Seq[Int] = { 
    val r = new Random(s) 
    (0 until n) map (v => r.nextInt(n))
  }
  def unique[A](l : Seq[A]) : Seq[A] = l.toSet.toSeq

  def addconst(l : Seq[Int]) : Seq[(Int, Unit)] = l map ( (_,())) 
  def comp(l : Seq[Int]) : Boolean = {
    val nl =  unique(addconst(l))
    nl.sorted == toSeq(fromSeq(nl) )
  }
  def all = { 
    val ls = (0 until 40) map (randSeq(_, 30))
    ls map comp
  }
}
