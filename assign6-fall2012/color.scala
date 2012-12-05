


object Coloring { 

  /* task 4.1
  Assume a set of Exams
  Set of students S. Each student s has a set of required exams R_s. find sequence of timeslots T of sets of exams at the same time X_t. ST 
  forall X_t in T, R_s in S. |X_t intersection R_s| <= 1 
  that is. students always are taking one or fewer exams. 

  let G be a graph (V,E) where V = Exams and (a,b) in E iff exists R_s in S. a in R_s and b in R_s
  assume we have a coloring function on G, c_G with Range C
  let T = { {e in Exams| c_G(e) == c} | c in C}

  let X_t in T, R_s in S
    assume |X_t intersect R_s| > 1 
      then exists a, b. a in X_t and b in X_t and a in R_s and b in R_s 
      so c_G(a) == c_G(b)
      so (a,b) not in E 
      but a in X_t and b in X_t 
      so (a,b) in E 
      contradiction, so 
    |X_t intersect R_s| <= 1 
    */
  type Vertex = Int
  type Graph = Map[Vertex, Set[Int]]

  def graphMIS(g : UGraph) : Set[vertex]= {}
  /*task 4.2
    
  graphColor meets the cost bounds because 
  if D is the maximum degree of the graph, 
  it can take a maximum of D + 1 iterations for the graph to be empty.
   */
  def graphColor(e : Seq[(Vertex, Vertex)]) : Seq[(Vertex,Int)] = {
    val colorings = graphColor_(makeGraph(e))
    colorset.toList.zipWithIndex.flatMap( (vs, c) => vs.toList.map( (_, c)))
  }
  
  def graphColor_(g : Graph) : Set[Set[Vertex]] =
    if (g.numNodes == 0)
      Set()
    else {
      val mis = graphMIS(g)
      graphColor(g - mis) + mis 
    }

}
