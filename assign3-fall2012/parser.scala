
object parser {

  sealed abstract class Tokenized
  case class Chunk(b : Int, e : Int) extends Tokenized
  case class Tokens(l : Int, s : Seq[Chunk], r : Int) extends Tokenized

  def tokens ( delim : Char => Boolean, input : String) : Seq[String] = {
    def toTokenized(c : Char, i : Int) : Tokenized = 
      if delim(c) 
        Tokens(i, Seq[Chunk](), i +1)
      else 
        Chunk(i, i + 1) 


    def merge(t0 : Tokenized, t1 : Tokenized) : Tokenized = {
      (t0, t1) match {
        case (Chunk(b0, e0), Chunk(b1, e1)) => Chunk(b0, e1)
        case (Chunk(b0, e0), Tokens(l, s, r)) => Tokens(b0, s, r)
        case (Tokens(l, s, r), Chunk(b1, e1)) => Tokens(l, s, e1)
        case (Tokens(ll, sl, rl), Tokens(lr, sr, rr)), => 
          Tokens(ll, 
            sl ++ if (rl == lr)
                    sr
                  else 
                    Chunk(rl, lr) +: sr,
            rr)
          
    }
    val ds = (input, (0 to input.length)).zipped.map(toTokenized)      
    val ts = ds.reduce(Tokens(0,Seq[Chunk](), 0))( merge)
    ts.s.map(c => input.substring(c.b, c.e))
  }
}
