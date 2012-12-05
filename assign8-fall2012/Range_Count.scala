import BSTOrderedTable._

object Range_Count {
  case class Point(x : Int, y : Int)


  abstract class Range_Count { 
    val table : Ord_Table[Int] 

    type CountTable 
    def makeQueryTable (ps : Seq[Point]) : CountTable
    def countInRange(t : CountTable)(p1 : Point, p2 : Point) : Int
  }
}
