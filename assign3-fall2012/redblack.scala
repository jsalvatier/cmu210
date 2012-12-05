trait RBColor
case object Red extends RBColor
case object Black extends RBColor

case class RBTreeMod[A](implicit ord : Ordering[A]) { 

  trait RBTree
  case object Empty extends RBTree
  case class Node (c : RBColor, l : RBTree, x : A, r : RBTree) extends RBTree

  def insert (tr : RBTree, v : A) : RBTree = { 
    def ins(t : RBTree) : RBTree = t match {
      case Empty => Node(Red, Empty, v, Empty)
      case Node(c, l,x,r) => ord.compare(v, x) match { 
        case -1 => balance(Node(c, ins(l), x, r           ))
        case  1 => balance(Node(c, l           , x, ins(r)))
        case  0 => Node(c, l, v, r) 
      }
    }
    blacken(ins(tr))
  }

  def blacken(t : RBTree) : RBTree = t match { 
    case Empty => Empty
    case Node(c, l,x,r) => Node(Black, l,x,r)
  }
  def f (x : A, y : A, z : A, a : RBTree, b : RBTree, c : RBTree, d : RBTree)= Node(Red, Node(Black, a, x, b), y, Node(Black, c, z, d))
  def balance(t : RBTree) : RBTree = t match {
    case Node(Black, Node(Red, Node(Red, a, x, b), y, c), z, d) => Node(Red, Node(Black, a, x, b), y, Node(Black, c, z, d))
    case Node(Black, Node(Red, a, x, Node(Red, b, y, c)), z, d) => Node(Red, Node(Black, a, x, b), y, Node(Black, c, z, d))
    case Node(Black, a, x, Node(Red, Node(Red, b, y, c), z, d)) => Node(Red, Node(Black, a, x, b), y, Node(Black, c, z, d))
    case Node(Black, a, x, Node(Red, b, y, Node(Red, c, z, d))) => Node(Red, Node(Black, a, x, b), y, Node(Black, c, z, d))
    case a => a
  }
  /*
  def merge(a : RBTree, b : RBTree, f : ( A => A => A)) : RBTree = a match { 
    case Empty => b
    case Node(c, l,x,r) => { 
      val (bl, br, y) = splitAt(b, x)
      val nx = y.map(f x) orElse x 
      balance(Node(c, merge(l, bl), nx, merge(r, br))) 
  */
  def addList(t : RBTree, l : List[A]) : RBTree = 
    l.foldLeft(t)(insert)
}
