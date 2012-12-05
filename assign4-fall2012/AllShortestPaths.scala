import scala.collection.parallel.immutable._

object AllShortestPaths {

  trait All_Shortest_Paths[A] { 
    type Edge = (A,A)
    type Vertex = A 
    type Graph

    type ASP

    def makeGraph(es : Seq[Edge]) : Graph

    def numEdges(g : Graph) : Int
    def numVerticies(g : Graph) : Int

    def outNeighbors (g : Graph )(v : Vertex) : Seq[Vertex]

    def makeASP( g : Graph)(v : Vertex) : ASP
    def report(a : ASP)(v : Vertex) : Seq[Seq[Vertex]]
  } 

  case class ASPVectorMap[A] extends All_Shortest_Paths[A] {

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

    def makeASP(g : Graph)(v : Vertex) : ASP = { 
      def makeASP_(x : Set[Vertex], f : Set[Vertex], asp : Graph) : Graph = f.size match { 
        case 0 => asp 
        case _ => { 
          val nx = x ++ f
          def backPath(w : Vertex) : Graph = { 
            val nNgh = g(w) -- nx 
            setToMap(_ => Set(w), nNgh)
          }
          val newPath = f.map(backPath).reduce(merge) 
          val nf = newPath.keySet
          val nasp = asp ++ newPath 
          makeASP_(nx, nf, nasp) 
        }
      }
      makeASP_(Set(), Set(v), Map(v -> Set())) 
    }
    def report(asp : ASP)(v : Vertex) : Seq[Seq[Vertex]]  = { 
        (asp get v) match { 
          case None => Seq()
          case Some(nvs) => nvs.size match { 
            case 0 => Seq(Seq())
            case _ => nvs.toSeq.flatMap(nv => report(asp)(nv).map(v +: _) )
          }
        }
    } 

    /* task2.6
    the maximum will occur when every path from one vertex to another is the same
    that occurs when there are a number of 'levels' between the start and end vertexes each point in a level is connected to each point in
    the next level and the previous level but not to any ahead or behind that. 

    if each level has w vertexes in it and there are l levels, then w * l = n and number of paths is |P| = w**l. 
    l = n / w
    so 
    |P| = w ** (n/w)
    = (w ** (1/w))**n
    the function w ** (1/w) reaches a maximum at w = e. 
    but w must be integer
    if w = 2 then (w ** (1/w)) = 2**.5 ~= 1.414 
    if w = 3 then (w ** (1/w)) = 1.44
    so 
    |P| = O(1.44**n)
    */

    
  }
}
