import Range_Count._
import BSTRangeCount._
import util._

object RangeCountTest { 
  def square(s : Int, n : Int) : Seq[Point] = {
    val r = new Random(1)
    val ns = 0 until n
    (r.shuffle(ns),r.shuffle(ns)).zipped.map(Point)
  }

  def count(ps : Seq[Point])(p1 : Point, p2 : Point) : Int =
    (ps filter (p => (p.x >= p1.x && p.x <= p2.x) && (p.y >= p1.y && p.y <= p2.y))).size

  def rangeSq(s : Int) : Int = {  
    val ps = square(s,30) 
    val p1 = Point(3,3)
    val p2 = Point(20, 25)
    countInRange(makeQueryTable(ps))(p1,p2) -  count(ps)(p1,p2)
  }
  def all = (0 until 20) map rangeSq 

}
