
import scala.collection.parallel.immutable._

object parser {

  sealed abstract class Tokenized 
  case class Chunk(b : Int, e : Int) extends Tokenized
  case class Tokens(l : Chunk, s : ParSeq[Chunk], r : Chunk) extends Tokenized

  def add(c : Chunk, d : Chunk) : Chunk = 
    Chunk(c.b, d.e)

  def tokens ( delim : Char => Boolean, input : String) : ParSeq[String] = {
    def toTokenized(c : Char, i : Int) : Tokenized = 
      if (delim(c)) 
        Tokens(Chunk(i,i), ParSeq(), Chunk(i+1,i+1))
      else 
        Chunk(i, i + 1) 


    def merge(t0 : Tokenized,  t1 : Tokenized ) : Tokenized = {
      (t0, t1) match {
        case (l : Chunk ,r : Chunk) => add(l, r) 
        case (l : Chunk, Tokens(rl, s, r)) => Tokens(add(l, rl), s, r)
        case (Tokens(l, s, lr), r : Chunk) => Tokens(l, s, add(lr, r))
        case (Tokens(ll, sl, rl), Tokens(lr, sr, rr)) => { 
          val c = add(rl, lr)
          val nsl = if (c.e == c.b) sl else sl :+ c
          Tokens(ll, nsl ++ sr, rr) 
        }
      }
    }
    val ds = (input, (0 to input.length)).zipped.map(toTokenized)      
    val ts = ds.reduce(merge)
    ts match { 
      case Chunk(b,e) => ParSeq(input.substring(b,e))
      case Tokens(l, s, r) => s.map(c => input.substring(c.b, c.e))
    }
  }
}
