import scala.math._
import scala.util._

object boruvaka {

  type Vertex = Int
  type Weight = Double
  type Edge = (Vertex, Vertex, Weight)

  trait MST_Solver { 
    def MST(t : (Seq[Edge], Int) ) : Seq[Edge];
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
    def MST(t : (Seq[Edge], Int)) : Seq[Edge] = { 
      val r = new Random(1)
      val (es, n) = t
      val ses = es.sorted( Ordering.by[Edge, Weight]( (_._3)  ))
      val nes = (es.zipWithIndex) map {case ((u, v, w), i) => (u, v, w, i)}

      val esl = MST_(n, Vector.tabulate(n)(i => i) , nes,List(), 0, r)
      esl flatMap (l => l map ( (e : EdgeL) => es(e._4) ))
    }
   
    def MST_(n : Int, vs : Seq[Vertex], es : Seq[EdgeL], t : List[Seq[EdgeL]], i : Int, r : Random) : List[Seq[EdgeL]] = {  
      if (es.size == 0) { 
        t
      }
      else { 
        val (c, p) = minStarContract(n, vs, es, i, r)
        val remapped = es map {case (u, v,w,l) => (p(u), p(v), w, l)} 
        val nvs = vs filter (v => p(v) == v) 
        val nne = remapped filter {case (u, v,w, l) => u != v} 
        MST_(n, nvs, nne, c ::t , i + 1, r)
      }
    }

    def minStarContract(n : Int, vs : Seq[Vertex], es : Seq[EdgeL], i : Int, r : Random)  : (Seq[EdgeL], Seq[Vertex])= {
      val heads = Vector.tabulate(n)( a => r.nextBoolean) 
      
      val mins = minEdges(n, vs, es)
      val contract = mins filter {case (u, v, w, l) => !heads(u) && heads(v)}

      val rm = Array.tabulate(n)(i => i)
      val remap = inject(contract map {case (u, v, w, l) => u -> v}, rm)
      (contract, remap) 
    }
    def minEdges(n : Int, vs : Seq[Vertex], es : Seq[EdgeL]) : Seq[EdgeL] = {
      val nes = es map { case e => (e._1, e)}
      val default = Vector.fill(n)((0,0,0.0,0))
      val edgs = inject(nes, default)
      vs map (v => edgs(v))
    }

    /* 
    assume this has work (|a| + |b|) and span (1)
    */
    def inject[A]( a : Seq[(Int, A)], b : Seq[A]) : Seq[A] = {
      var r = b.toBuffer
      for ( t <- a) { 
        val (i, v) = t
        r(i) = v
      }
      r
    }
  }
  
  /* task 3.1
  let G be a weighted graph and  |G.E| > 2. with distinct weights 
    prove that the second smallest edge in G.E is in MST(G)

    let (a,b) in E be the smallest edge weight and 
        (c,d) in E be the second smallest edge weight 
        (a,b) != (c,d) since otherwise they would be the same edge
        thus either c not in {a,b} or d not in {a,b}

          case c not in {a,b}
            thus (a,b) is not an edge of c, so since (c,d) is the second minimum, 
            (c,d) is the minimum edge of c and thus in MST(G)
          case d not in {a,b} 
            similar reasoning applies 

          (c,d) in MST(G)
          QED

  task 3.2

    let V be a ring with +1 and -1 defined
    let
    r_v ~ U(0,1) iid 
    A_v = if (r_v > r_{v-1} and r_v > r_{v+1} then 1 else 0 
    f = sum_{v in V}( A_v)
      
      calculate E(f)

      E(f) = E(sum_{v in V}( A_v))
           = sum_{v in V} E(A_v)
      
      assume r_v fixed
        E(A_v) = P(r_v > r_{v-1} and r_v > r_{v+1})
             = P(r_v > r_{v-1}) * P(r_v > r_{v+1})

             = r_v * r_v
             = r_v**2
      E(A_v) = integral_[0,1](r_v**2 d{r_v}) 
             = r_v**3/3 eval[0,1] 
             = 1/3

      E(f) = sum_{v in V} E(A_v)
           = sum_{v in V} 1/3
           = n/3

  task 3.3
      
}
