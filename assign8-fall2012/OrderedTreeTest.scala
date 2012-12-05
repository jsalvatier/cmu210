import BSTOrderedTable._

object OrderedTableTest { 

  val t = BSTOrderedTable[Int](implicitly[Ordering[Int])
  import t._

  def randList(s: Int, n : Int ) : List[Int] = { 
    val r = new Random(s) 
    val l = (0 until n) map (v => r.nextInt(n))
    r.sorted
  }

  def comp(l : List[Int]) : Boolean = {
    val nl =  l map ( (_, ()))
    nl == fromList(nl) 
  }
  def all = { 
    val ls = (0 until 10) map (randList(_, 30))
    ls map comp
  }

  

    
    
}
