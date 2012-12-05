

object bridges { 

  type Vertex = Int 
  case class UGraph(vs : Array[Set[Vertex]], edges : Seq[(Vertex, Vertex)] )
  
  def getNeighbors(g : UGraph)(v : Vertex) : Set[Vertex] = g.vs(v)
  def getVertexes(g : UGraph) = (0 to g.size)

  case class State(vis : Map[Vertex, Int], highback : Option[Vertex],  bridges : List[(Int, Int)])
  
  def findBridges( g : UGraph) : Seq[(Int, Int)] = getVertexes(g).foldLeft((true, Set[Vertex](), List[(Int,Int)]()))  (checkFind(g))._3

  def bridgeEdge(g : UGraph)(a : Vertex)(s : State, b : Vertex) : State = { 
    if (s.vis contains b)
      // obviously in a loop
      State(s.vis, Some(b), s.bridges)
    else {
      val ns = State(s.vis + b, s.loopv, s.bridges)
      val nns = getNeighbors(g)(b).foldLeft(ns)(bridgeEdge(g)(b) _) 

      val brs = if (nns.loopv.size == 0) 
                  (a,b)::nns.bridges
                else 
                  nns.bridges

      State(nns.vis, nns.loopv - b, brs)
    }
  
  }
} 
