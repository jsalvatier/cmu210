
object distance {
  //task 2.1 multiple sources 

  
  def djs_multiple( g : Graph, s : Vertex) : Map[Vertex, Double]= {
    def djsm(x : Map[Vertex, Double], q : PriorityQueue[Vertex]) : Map[Vertex, Double] = q.deleteMin match { 
      case None => x
      case Some((v, q) => {
        if (x.contains(v)) 
          x 
        else 
          x
      }}

}
