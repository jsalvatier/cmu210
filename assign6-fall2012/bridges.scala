

object bridges { 

  type Vertex = Int 
  type UGraph = Array[Set[Vertex]]
  
  def getNeighbors(g : UGraph)(v : Vertex) : Set[Vertex] = g(v)
  def getVertexes(g : UGraph) = (0 to g.size)

  type State = (Set[Vertex], List[(Int, Int)])
  def findBridges( g : UGraph) : Seq[(Int, Int)] = getVertexes(g).foldLeft((true, Set[Vertex](), List[(Int,Int)]()))  (checkFind(g))._3

  def checkFind(g : UGraph, n : Vertex)( t : (Boolean, Set[Vertex], List[(Int, Int)]), v : Vertex) : (Boolean, Set[Vertex], List[(Int, Int)]) = {
        val (allb, nnx, brs) = findBridges_(g)(true, t._2, t._3)(v)
        val nallb = if (t._1) allb else t._1
        (nallb, nnx, (n,v)::brs)
      }

  private def findBridges_(g : UGraph)(allb : Boolean, x : Set[Vertex], bridges : List[(Int, Int)])(n : Vertex) : (Boolean, Set[Vertex],
    List[(Int, Int)]) = { 
    if (x contains n) 
      (false, x, bridges)
    else {
      val nx = x + n 
      val ngb = getNeighbors(g)(n) 
      ngb.foldLeft((true, x, bridges))(checkFind(g, n) _ )
    }
  }

    

    


} 
