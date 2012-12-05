

object PriorityQueueLibrary {
  abstract class PriorityQueueLib[A](implicit o :Ordering[A]) { 

    type PQueue
    val empty : PQueue
    def insert(q : PQueue, v : A) : PQueue
    def deleteMin (q : PQueue) : Option[(PQueue, A)]

    def queueString( q : PQueue) : String = (deleteMin(q)) match { 
      case None => ""
      case Some((nq, a)) => a.toString ++ " ," ++ queueString(nq)
    }

   
  }

  case object Ord { 
    def derOrdering[A,B](fn : B=>A)(implicit o : Ordering[A]) : Ordering[B] = { 
      case object Ord extends Ordering[B] {
        def compare(a:B, b:B) = o.compare(fn(a), fn(b)) 
      }
    Ord
    }
  }
  sealed class LeftHeapPQueueLib[A]()(implicit o : Ordering[A]) extends PriorityQueueLib[A] {

    abstract sealed case class Heap
    case object Empty extends Heap
    case class Node(h : Int, l : Heap, x : A, r : Heap) extends Heap


    type PQueue = Heap
    val empty = Empty 
    
    def single(v : A) : Heap = makeN(empty, v, empty)
    def insert(q : Heap , v : A) : Heap = merge(q, single(v)) 

    def rank(a : Heap) : Int = a match { 
      case Empty => 0 
      case Node(h,_,_,_) => h 
    }
    def makeN(l : Heap, x : A, r : Heap) : Heap= 
      if (rank(l) >= rank(r)) 
        Node(rank(r) + 1, r, x, l)
      else 
        Node(rank(l) + 1, l, x, r) 

    /* 
    
    x : A <= b : Heap if 
      b is Empty or b is Node(_,_,y,_) and x <= y

    heap property. 
      Empty heap
      Node(_, l, x, r) heap if x <= l  and x <= r 
    
    prove makeN preserves leftist property

    assume l, r as leftist. 
      case l.r >= r.r 
        then node(_,l, x, r) leftist 
      case l.r < r.r 
        r.r <= l.r
        then node(_, r, x, l) leftist  
    */
    def merge(a : PQueue, b : PQueue) : PQueue = (a,b) match {
      case (a, Empty) => a 
      case (Empty, b) => b
      case (Node(_, al, ax, ar),Node(_, bl, bx, br)) =>
        if (o.lt(ax, bx))  
          makeN(al, ax, merge(ar, b))
        else 
          makeN(bl, bx, merge(br, a))
    }
      
    def deleteMin(a : Heap) : Option[(Heap, A)] = a match { 
      case Empty => None
      case Node(_, l, x, r) => Some((merge(l, r), x))
    }

    /* 

    prove forall x, y: A. forall total expr a,b : Heap. a <= b -> a <= makeT(a, y, b)
      case a.r <= b.r 
        makeT(a,y,b) == Node(b.r, b, y, a)
        x <= y so x <= Node(b.r, b, y,a) 
        x <= makeN(a,y,b)
      case b.r < a.r 
        makeT(a, y,b) == Node(a.r, a, y, b)
        x <= y so x <= Node(a.r, a, y, b)
        x <= makeN(a,y,b)


    prove forall x : A, a,b : Heap. x < a and x <= b -> x <= merge(a,b) 
      let x:A, a,b:Heap. assume x<= a and x <= b 
        merge(a,b) == (a,b) match {} 
        case (a, Empty) 
          merge(a,b) == a 
          x <= a 
          x <= merge(a,b)
        case (Empty, b) 
          merge(a,b) == b 
          x <= b 
          x <= merge(a,b)
        case (node(_, al, ax,ar), node(_, bl, bx, br))
          case ax <= bx 
            merge(a,b) == makeN(al,ax, merge(ar,b)) 
            x <= a so x <= ax so x <= makeN(al, ax,merge(ar,b))
            x <= merge(a,b)
          case bx < ax 
            merge(a,b) == makeN(bl, bx, merge(br,a))
            x <= a so x <= ax 
            x <= makeN(bl,bx,merge(br,a)) 
            x <= merge(a,b)
          x <= merge(a,b)
        x <= merge(a,b)

          
    prove merge a b  preserves heapity


    structural induction on (a,b). assume merge perserves heapity for heaps smaller than (a,b) 
    
      case (a, Empty) then merge(a,b) = a, and a heap 
      case (Empty, b) then merge(a,b) = b and b heap
      case (Node(_, al, ax, ar), Node(_, bl, bx, br)) then 
        either ax <= bx or bx < ax 
          case ax<= bx 
            merge(a,b) == makeN(al, ax, merge(ar,b))
            a heap so al heap and ax <= al  
            ax <= ar and ax <= bx so ax <= b 
            so ax <= merge(ar, b)
            merge(ar, b) heap by IH 
            so merge(a, b) heap 

          case bx < ax 
            merge(a,b) == makeN(bl, bx, merge(br,a))
            b heap so bl heap and bx <= bl  
            bx <= br and bx <= ax so bx <= a 
            so bx <= merge(br, a)
            merge(br, a) heap by IH 
            so merge(a, b) heap 

        merge(a,b) heap

     */
    /* okasaki 
     * exercise 3.1
    prove the right spine contains at most floor(log(n + 1 )) elements 
       
    */
  }
}
