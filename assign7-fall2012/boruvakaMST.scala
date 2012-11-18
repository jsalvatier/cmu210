


object boruvaka {

  type Vertex = Int
  type Weight = Double
  type Edge = (Vertex, Vertex, Weight)

  trait MST_Solver { 
    def MST(es : Seq[Edge], n : Int) : Seq[Edge];
  }

  object MST_Map extends MST_Solver {
    type Graph[L] = Map[Vertex, Map[Vertex, Double,L]] 
    type G = Graph[Edg]
    type E = Edge[Edg] 

    def makeGraph[L]( es :  Seq[Edge] ) : Graph[L] = 
      (es map single) reduce merge

    def MST(es : Seq[Edg], n : Int) : Seq[Edg] = {
      val r = new Random(1)
      val labeled = es map {case Edg(a,b,w,_) => Edge[Edg](a,b,w, Edg(a,b,w,Unit)) }
      MST_(makeUGraph(labled), r)
    }

    def MST_(g : G, r : Random) : G = { 
      if (g.size == 0) 
        g
      else {
        val min_ed = minEdge(g) 
        val (ng, edges) = starContract(g, min_ed, r)
        val original_edges = edges map (_.2)
        MST_(ng,r ) ++ makeGraph(original_edges)
      }

    }
    def starContract(g : Graph[L], es : Set[Edge[L]], r : Random) : (Graph[L], Set[Edge[L]]) = { 
      val heads = Vector.tabulate(g.size)(r.nextBoolean)
      val ces = es.filter {case ((a,b, w), _) => !heads(a) && heads(b)}
      val v = Map(ces map {case ((a,b,w), _) => a -> b})
      (merge(g)(v), ces)
    }

    def merge(g : G, mergeto : Map[Vertex, Vertex]) : G = {
      val ngraph = mergeto map { case (v, nv) => nv -> getNeighbors(g)(v)}  
      merge((g - mergeto.domain), ngraph)(mergeset)
    }


    def minEdge(g : G) = g.map( (k,v) => v.reduce(smallest) )
    def min(a :E, b : E) : E = 
      if (a.w < b.w) 
        a
      else 
        b
  }
}
