sealed abstract class Paren
case object OParen extends Paren
case object CParen extends Paren


object Parens {
  def toNum(v : Paren) : Int = v match {
    case OParen => 1
    case CParen => -1
  }
}
