
import WGraphing._
import boruvaka._
import scala.util._

object MSTTest { 
  type Graph = Seq[Edge]
  type MSTinput = (Graph, Int)
  type MSTresult = Graph

 
  def uEdge(a : Int, b :Int, w : Double): Graph = List((a,b, w),(b,a,w))

  def lineGraph(sz : Int) : (Graph, Int) =
    (
      (0 until sz) flatMap (i => uEdge(i , i+1, .5 +i*.01)), 
      sz + 1
    )
  def starGraph(sz : Int) : (Graph, Int) =
    (
      (1 until sz) flatMap (i => uEdge(0, i, .5)) ,
      sz 
    )

  def randomGraph(sz : Int) : (Graph, Int) = {
    val r = new Random(1)
    val g = (1 until sz) flatMap (i => uEdge(i, r.nextInt(i), r.nextDouble))
    (g, sz )
  }

  val corners = List(lineGraph _, starGraph _, randomGraph _)

  def testGraphs(n : Int) : List[MSTinput] = corners map (fn => fn(n))

  def reachable (g : WGraph, v : Int) : Set[Int] = { 
    def dfs_(x : Set[Int], f : Set[Int]) : Set[Int] ={
      if (f.size == 0)
        x
      else { 
        val nx = x ++ f
        val ngs = (f map (g(_).keySet)) reduce (_ ++ _)
        val nf = ngs -- nx
        dfs_(nx, nf) 
      }
    }
    dfs_(Set(), Set(v))
  }

  def spanning (i : MSTinput, r : MSTresult) : Boolean = {
    val nr = r flatMap (e => uEdge(e._1, e._2, e._3))
    val s = reachable( makeWGraph(nr), 0)
    all((0 until i._2) map (s contains _)) 
  }

  def all(v : Seq[Boolean]) : Boolean = 
    if (v.size == 0)
      true
    else 
      v.reduce( _ && _)

  def grader(i : MSTinput, r : MSTresult) : Boolean = 
    spanning(i,r)
    
  def all() : Boolean = 
    all(testGraphs(4) map (a => grader(a, MST_Seq.MST(a))  ))
  
}
