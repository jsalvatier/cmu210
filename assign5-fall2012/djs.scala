import PriorityQueueLibrary._
import WGraphing._

object djslib {
  implicit val tupOrder = Ordering.by[(Vertex,Double), Double](_._2) 
  val pqLib : PriorityQueueLib[(Vertex, Double)] = new LeftHeapPQueueLib[(Vertex,Double)]()
  import pqLib._


  def djs(g : WGraph, s : Vertex, e : Vertex) : Option[Double]  =  
    djs_(g, Set(), insert(empty, (s, 0)), e) 

  def djs_(g : WGraph, x : Set[Vertex], vs : PQueue, e : Vertex) : Option[Double]=
      (deleteMin(vs)) match { 
        case None => None 
        case Some((nvs, (v, d))) => 
          if (e == v)
            Some(d)
          else {
            val nx = x + v
            val nnvs = g(v).foldLeft(nvs)( 
              { case (vs_ , (ngh, w)) => insert(vs_, (ngh, d + w)) }
               )

            djs_(g, nx, nnvs, e)
          }
      }
  val testgraph = makeWGraph(List((0, 1, 5.),(0,2,2.),(0,3,4.),(2,4,1.), (2,5,5.),(3,2,1.), (3,6,2.), (4,1,1.),(6,5,0)))


  private val augs : Vertex = -100
  private val auge : Vertex = -101
  def augStart(s : Set[Vertex])(g : WGraph) : WGraph = {
    val edges = s.toList.map(v => (augs,v,0.0))
    g + makeWGraph(edges) 
  }
  def augEnd(s : Set[Vertex])(g : WGraph) : WGraph = {
    val edges =s.toList.map(v => (v,auge,0.0))
    val g2 : WGraph = makeWGraph(edges) 
    g + g2 
  }

  def djsm(g : WGraph, s : Set[Vertex], e : Vertex) : Option[Double] = 
    djs(
      augStart(s)(g),
      augs, e)
  // task2.2
  def djsmm(g : WGraph, s : Set[Vertex], e : Set[Vertex]) : Option[Double] = 
    djs(
      (augStart(s) _ compose augEnd(e) _)(g),
      augs, auge)

    /* task 2.3
  heuristic distance to target vertex 
  Lemma : consistency
  h(w) = dist(w,T)
  let (a,b) be an edge
    dist(a, T) <= dist(a, b) + dist(b,T)
    by the triangle inequality
    h(a) <= w(a,b) + h(b)
    h(a) - h(b) <= w(a,b)
  */

 /* task 2.4
 if h(w) = 0 then it will always do the same thing as djs
 */

 /* task 2.5
 prove if consistency holds then A* works correctly

  by induction on the size of (V-X) 
    case |V - X| = 0 
       

  let (V, E, w) be a wgraph and h : V -> R_+ be a consistent heuristic
  let s be the startin point 
    let X subset V and s in X 
    let s = argmin{ a in (V-X) } ( 
      
*/


  def astar(g : WGraph, s : Vertex, e : Vertex, h : (Vertex => Double)) : Option[Double] =
    astar_(g, Set(), insert(empty, (s,0.0)), e , h)

  def astar_(g : WGraph, x : Set[Vertex], vs : PQueue, e : Vertex, h : (Vertex => Double)) : Option[Double]=
      (deleteMin(vs)) match { 
        case None => None 
        case Some((nvs, (v, d))) => 
          if (e == v)
            Some(d)
          else {
            val nx = x + v
            val nnvs = g(v).foldLeft(nvs)( { 
              case (vs_ , (ngh, w)) => 
                insert(vs_, (ngh, d + w + h(ngh)))
              })

            astar_(g, nx, nnvs, e, h)
          }
      }
  def dist(l : Map[Vertex, (Double, Double)])(a : Vertex)(b : Vertex) : Double = {
    val ((ax, ay), (bx, by)) = (l(a), l(b)) 
    math.sqrt((ax- bx)^2 + (ay-by)^2)
    }
  val locs = Map(1 -> (1,3.5), 2 -> (3,6), 3 -> (5,5), 4 -> (1,2), 5 -> (3.5,3.5), 6 -> (5,1), 7 -> (3,2), 8 -> (4,4.5))
  val edgs = List((1,2), (2,3), (1,4), (4,5), (2,5), (5,7), (5,6), (3,8), (3,6))
  val ldist = dist(locs) _
  val tgraph = makeWGraph(edgs.map({ case (a,b) => (a,b, ldist(a)(b))}))



    
   }