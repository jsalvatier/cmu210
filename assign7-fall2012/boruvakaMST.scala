


object boruvaka {

  type Vertex = Int
  type Weight = Double
  type Edge = (Vertex, Vertex, Weight)

  trait MST_Solver { 
    def MST(es : Seq[Edge], n : Int) : Seq[Edge];
  }

  object MST_Seq extends MST_Solver {
    /* cost bounds : 
    the cost bounds specified are 
      W <= m log n + n 
      S <= (log n)**k for some k 

    but the sorting of the edge list they suggest plainly violates the cost bounds unless you use some kind of non-comparison sort (which
      has not been covered)

    thus we assume that it's going to be the same cost bounds EXCEPT for the initial sort 
     */
    type EdgeL = (Vertex, Vertex, Weight, Int)
    def MST(es : Seq[Edge], n : Int) : Seq[Edge] = { 
      val ses = es.sorted( by[Edge, Weight](_.3))
      val nes = es map {case (u, v, w) => (u, v, w, i)}

      val esl = MST_(  , nes,List(), 0)
      esl flatmap (l => l map ( es(_) )
    }
    type VVMap = Array[Vertex]
    def MST_(vs : Array[Vertex], es : Seq[EdgeL], t : List[List[Int]], i : Int) : List[List[Int]] =  
      if (e.size == 0) 
        t
      else { 
        val (c, p) = minStarContract(vs, es, i)
        val remapped = map {case (u, v,w,l) => (p(u), p(v), w, l)} 
        val nne = remapped filter {case (u, v,w, l) => u != v} 
        MST_(n, nne, c ::t , i + 1)
      }

    def minStartContract( vs : Array[Vertex], es : Seq[EdgeL], i : Int)  : (Seq[EdgeL], VVMap)= {
      val heads = Vector.tabulate(vs.size)( n => i * 71 % 2 == 0) 
      
      val mins = minEdges(vs, es)
      val contract = mins filter {case (u, v, w, l) => !heads(u) && heads(v)}

      val rm = Array.tabulate(vs.size)
      val remap = rm update (contract map {case (u, v, w, l) => u -> v})
      (contract, remap) 
    }
    def minEdges(vs : Array[Vertex], es : Seq[EdgeL]) : Seq[EdgeL] = {

    }
  }
  object MST_Map extends MST_Solver {
    type Graph = Seq[Seq[Vertex]] 

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
      val heads = Vector.tabulate(g.size)(r.nextBoolean) // assume O(n) work  O(log n) span
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
