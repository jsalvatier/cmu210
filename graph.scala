import scala.collections.immutable

type Graph = Vector[List[Int]]

class Tree[A] (v : A, r : Forest[A])
type Forest[A] = List[Tree[A]]

object graph {
  def depthfirst (g : Graph, n : Int) : Tree[Int] = dfs(g, n, Set.empty, Queue.enqueue(Queue.empty, n))
  def dfs_t (g : Graph, visited : Set[Int], ordered : Queue[Int]) : (Tree[Int], Set[Int], Queue[Int]) = { 
    val nordered, n = ordered.dequeue 
    nvisited = visited + n 
    for (no <- g[n]){
      if (!(nnvisited contains no))
        nnordered = nnordered.enqueue(no) 
    }
  }
  def depthfirst_imp(g : Graph, n : Int) = { 
    var ordered = Queue.enqueue(Queue.empty, n)
    var visited = Set.empty 
    while (!ordered.empty){
      ordered, v = ordered.dequeue
      if (!ni.contains(n)) {
        visited = visited + v
        for (ni <- g[v]) 
            ordered = ordered.enqueue(ni)   
      }
    }
  } 
def depthfirst(g : Graph, n : Int) = { 
    var ordered = Queue.enqueue(Queue.empty, n)
    var visited = Set.empty 
}
def dfs_(g : Graph , n : Int, ordered : Queue, visited : Set) {
    while (!ordered.empty){
      ordered, v = ordered.dequeue
      if (!ni.contains(n)) {
        visited = visited + v
        for (ni <- g[v]) 
            ordered = ordered.enqueue(ni)   
      }
    }
  } 

}
