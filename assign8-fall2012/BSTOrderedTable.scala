

object BSTOrderedTable { 

  abstract class Ord_Table[K]{ 
    type Table[V]
    def size[V](t : Table[V]) : Int
    def last[V](t : Table[V]) : Option[(K, V)]
    def first[V](t : Table[V]) : Option[(K,V)]
    def split[V](t : Table[V], a : K) : (Table[V], Option[(K, V)], Table[V])
    def join[V](t1 : Table[V], a : Option[(K, V)], t2 : Table[V]) : Table[V]
    def getRange[V](t : Table[V], a : K, b : K) : Table[V]
    def empty[V] : Table[V]

    def foldLeft[V, B](t : Table[V])(i : B)(f : (B, (K,V)) => B) : B  
    def foldRight[V,B](t : Table[V])(i : B)(f : (B, (K,V)) => B) : B


    def insert[V](t : Table[V])(k : K, v : V) : Table[V] = {
      val (l, _, r) = split(t, k)
      join(l, Some((k,v)), r)
    }
    def delete[V](t : Table[V])(k : K) : Table[V] = {
      val (l, _, r) = split(t, k)
      join(l, None, r)
    }

    
    
    def fromSeq[V](l : Seq[(K, V)]) : Table[V] =  
      l.foldLeft(empty[V])( (t,kv) => insert(t)(kv._1,kv._2))

    def toSeq[V](t : Table[V]) : Seq[(K, V)] = 
      foldRight(t)(List[(K,V)]())( (l, kv) => kv ::l)

  }

  case class BSTOrderedTable[K](o : Ordering[K]) extends Ord_Table[K] { 
    trait BST[+V]
    case class Node[+V](l : BST[V], k : K, v : V, r : BST[V], s : Int) extends BST[V]
    case object Leaf extends BST[Nothing]

    type Table[V] = BST[V]
    def size[V](t : Table[V]) : Int = t match { 
      case Leaf => 0
      case Node(_,_,_,_,s) => s
    }
    def makeN[V](l : Table[V], k : K, v : V, r : Table[V]) : Table[V] = 
      Node(l, k, v, r, size(l) + size(r) + 1)

    def last[V](t : Table[V]) : Option[(K,V)] = t match {
      case Leaf => None
      case Node(_ , k, v, Leaf, _) => Some((k,v))
      case Node(_, _, _, r, _) => last(r)
    }

    def first[V](t : Table[V]) : Option[(K, V)] = t match { 
      case Leaf => None 
      case Node(Leaf, k, v, _, _) => Some((k,v))
      case Node(l, _, _, _, _) => first(l)
    }

    def split[V](t : Table[V], a: K): (Table[V], Option[(K, V)], Table[V]) = t match { 
      case Leaf => (Leaf, None, Leaf)
      case Node(l,k, v, r, _) => o.compare(a, k) match {
        case -1 => {
          val (l2, kv2, r2) = split(l, a)
          (l2, kv2, makeN(r2,k, v, r))
        }
        case 0 => (l, Some((k,v)), r)
        case 1 => {
          val (l2, kv2, r2) = split(r, a)
          (makeN(l,k, v, l2), kv2, r2)
        }
      }
    }

    def single[V](k : K, v : V) : Table[V] = makeN(empty, k,v, empty)

    def join[V](t1 : Table[V], m : Option[(K,V)], t2 : Table[V]) : Table[V] = m match { 
      case None => merge(t1, t2)
      case Some((k,v)) => merge(t1, merge(single(k,v), t2))
    }

    def merge[V](t1 : Table[V], t2 : Table[V]) : Table[V] = (t1, t2) match { 
      case (Leaf, _) => t2
      case (_, Leaf) => t1
      case (Node(l1,  k1, v1, r1, _), Node(l2, k2, v2, r2, _)) =>
        if (k1.hashCode > k2.hashCode)
          makeN(l1, k1, v1, merge(r1, t2)) 
        else 
          makeN(merge(t1, l2), k2, v2, r2)
    }


    def insertOpt[V](t : Table[V])(kv : Option[(K,V)]) = kv match { 
      case None => t 
      case Some((k,v)) => insert(t)(k, v)
    }

    def getRange[V](s : Table[V], k1 : K, k2 : K) : Table[V] = {
      val (_, v1, ns) = split(s, k1)
      val(nns, v2, _) = split(insertOpt(ns)(v1), k2)
      insertOpt(nns)(v2)
    }
    def empty[V] : Table[V]= Leaf

    def foldLeft[V, B](t : Table[V])(i : B)(f : (B, (K,V)) => B) : B = t match { 
      case Leaf => i
      case Node(l, k, v, r,_) => {
        val lr = foldLeft(l)(i)(f)
        val mr = f(lr, (k,v))
        foldLeft(r)(mr)(f)
      }
    }
    def foldRight[V, B](t : Table[V])(i : B)(f : (B, (K,V)) => B) : B = t match { 
      case Leaf => i
      case Node(l, k, v, r,_) => {
        val rr = foldRight(r)(i)(f)
        val mr = f(rr, (k,v))
        foldRight(l)(mr)(f)
      }
    }

  }
}
    
      
