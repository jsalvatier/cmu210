import scala.collection.immutable

object Graphing {
    type Graph = Map[Vertex, Set[Vertex]]

    type ASP = Graph 

    def unionWith[A,B](f : (B,B) => B)(a : Map[A, B], b : Map[A, B]) : Map[A, B] = 
      (a -- b.keySet) ++ b.map {
        case (k, v) => k -> (a get k map { f(v,_)} getOrElse v)
      }

    val merge  = unionWith(_ ++ _) : (Graph, Graph) => Graph
    /*
    es.map(singleEdgeGraph) work is O(|es|) span is O(1)
    .reduce
      work is O(|es| + sum_{f(x,y) in O_r(f, es)} (W(unionWith(_ ++ _) (x,y))))
        |es| + sum_{f(x,y) in O_r(f, es)} ( m lg((n + m)/m))
        with m = min(|x|, |y|)
             n = max(|x|, |y|)
        == |es| + sum_{f(x,y) in O_r(f, es)} (|x|)
        == |es| + |es|log(|es|)
        == |es|log(|es|)
      S(makeGraph) == lg(|es|) max_{f(x,y) in O_r(f, es)} (S(unionWith(_ ++ _) (x,y)))
          s(unionwith (_ ++ _) (x,y)) == O(lg (|x| + |y|))
       == lg(|es|) max_{f(x,y) in O_r(f,es)} lg (|x| + |y|) 
       == lg(|es|) lg(|es|)
       == lg(|es|)**2
    
    */
    def singleEdgeGraph(e : Edge) : Graph = { 
      val (a,b) = e 
      Map(a -> Set(b), b -> Set())
    }
    def makeGraph( es : Seq[Edge]) : Graph = {
      es.map(singleEdgeGraph).reduce(merge)
    }
     
    def numEdges( g : Graph) : Int = 
      g.map(_._2 size).sum

    def numVerticies( g : Graph ) : Int = g.size

    def outNeighbors ( g : Graph)(v : Vertex) : Seq[Vertex] = g(v).toSeq

    def setToMap[A,B] (f : (A => B), a : Set[A]) : Map[A,B] ={ 
      def single(ai : A) : Map[A,B] = Map(ai -> f(ai))
      a.size match { 
        case 0 => Map[A,B]()
        case _ => a.map(single).reduce(_ ++ _)
      }
    }
}
