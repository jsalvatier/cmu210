import scala.collection.immutable

object WGraphing {
    type Vertex = Int 
    type WGraph = Map[Vertex, Map[Vertex, Double]]

    type Edge = (Vertex,Vertex, Double)

    def unionWith[A,B](f : (B,B) => B)(a : Map[A, B], b : Map[A, B]) : Map[A, B] = 
      (a -- b.keySet) ++ b.map {
        case (k, v) => k -> (a get k map { f(v,_)} getOrElse v)
      }

    val merge = unionWith(_ ++ _) : (WGraph, WGraph) => WGraph
    
    def singleEdgeWGraph(e : Edge) : WGraph = { 
      val (a,b,w) = e 
      Map(a -> Map(b -> w), b -> Map())
    }
    def makeWGraph( es : Seq[Edge]) : WGraph = {
      es.map(singleEdgeWGraph).reduce(merge)
    }
     
    def numEdges( g : WGraph) : Int = 
      g.map(_._2 size).sum

    def numVerticies( g : WGraph ) : Int = g.size

    def outNeighbors ( g : WGraph)(v : Vertex) : Seq[(Vertex, Double)] = g(v).toSeq

    def setToMap[A,B] (f : (A => B), a : Set[A]) : Map[A,B] ={ 
      def single(ai : A) : Map[A,B] = Map(ai -> f(ai))
      a.size match { 
        case 0 => Map[A,B]()
        case _ => a.map(single).reduce(_ ++ _)
      }
    }
}
