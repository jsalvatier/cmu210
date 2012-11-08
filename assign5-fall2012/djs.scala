import PriorityQueueLibrary._
import WGraphing._

object djslib {
  implicit val tupOrder = Ordering.by[(Vertex,Double), Double](_._2) 
  val pqLib : PriorityQueueLib[(Vertex, Double)] = new LeftHeapPQueueLib[(Vertex,Double)]()
  import pqLib._


  def djs(g : WGraph, fv : Vertex) : Map[Vertex, Double] =  
    djs_(g, Map(), insert(empty, (fv, 0))) 

  def djs_(g : WGraph, x : Map[Vertex, Double], vs : PQueue) : Map[Vertex, Double] = 
      (deleteMin(vs)) match { 
        case None => x
        case Some((nvs, (v, d))) => {
          val nx = if (x.contains(v)) 
                     x 
                   else 
                     x + ((v, d))
          val nnvs = g(v).foldLeft(nvs)( 
              { case (vs_ : PQueue, (ngh : Vertex, w : Double)) => 
                  insert(vs_, (ngh, d + w))
              } 
            )
          djs_(g, nx, nnvs)
          }
      }

  val testgraph = makeWGraph(List((0, 1, 5.),(0,2,2.),(0,3,4.),(2,4,1.), (2,5,5.),(3,2,1.), (3,6,2.), (4,1,1.),(6,5,0)))


  private val augs : Vertex = -100
  def augmentStart(g : WGraph, vs : Set[Vertex]) : WGraph =
    g + ((augs, setToMap[Vertex, Double](v => 0.0, vs)))

  def djsm(g : WGraph, vs : Set[Vertex]) : Map[Vertex, Double] = 
    djs(augmentStart(g, vs), augs) - augs 

  // task2.2
  def djsmm(g : WGraph, vs : Set[Vertex], ve : Set[Vertex]) : Option[Double] = 
    djsmm_(augmentStart(g, vs), Set(), insert(empty, (augs, 0)), ve)

  def djsmm_(g : WGraph, x : Set[Vertex], vs : PQueue, ve : Set[Vertex]) : Option[Double] = 
      (deleteMin(vs)) match { 
        case None => None 
        case Some((nvs, (v, d))) => 
          if (ve contains v)
            Some(d)
          else {
            val nx = x + v
            val nnvs = g(v).foldLeft(nvs)( { 
              case (vs_ , (ngh, w)) => 
                insert(vs_, (ngh, d + w))
              })

            djsmm_(g, nx, nnvs, ve)
          }
      }

  /* task 2.3
  heuristic distance to target vertex 
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
*/
}
