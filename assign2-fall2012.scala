import scala.collection.parallel.immutable 
import scala.math._ 
import scala.util._ 

case class Bld (l : Double, r : Double, h : Double) 
case class Line(x : Double, h : Double) 
type Profile = Vector[Line] 

object sky { 
  def skyline(l : Vector[Bld]) : Profile = clean(l.map(toProfile)) 
  
  def clean(l : Vector[Profile]) : Profile =  { 
    if (l.length > 1) { 
      val (a, b) = l.splitAt(l.length/2) 
      merge(0,0, clean(a),clean(b)) 
    } 
    else 
      l(0)
    } 
    
  def toProfile (b : Bld) : Profile= Vector[Line]( Line(b.l, b.h),
                                        Line(b.r, 0))
  def merge (a0 : Double, b0 : Double, ap0 : Profile, bp0 : Profile) : Profile ={ 
    if (ap0.isEmpty && bp0.isEmpty)
      Vector[Line]()
    else {
      val (x, a, b, ap, bp) = if (bp0.isEmpty || (ap0.nonEmpty && ap0.head.x < bp0.head.x)) 
                                (ap0.head.x, ap0.head.h, b0, ap0.tail, bp0) 
                              else
                                (bp0.head.x, a0, bp0.head.h, ap0, bp0.tail)
      if (max(a,b) == max(a0, b0))
        merge(a, b,ap, bp)  
      else
        Line(x, max(a, b)) +: merge(a, b, ap, bp) 
    }
  }
  def rand_blds(n : Int, r : Random) : Vector[Bld] = { 
    if (n > 0) {  
      val x = r.nextDouble() * 10
      val d =r.nextDouble() *2
      val bld = Bld(x ,x + d , r.nextDouble())
      rand_blds(n-1, r) :+ bld 
    }
    else 
      Vector[Bld]()
  }

  def any(a : Seq[Boolean]) : Boolean = a.exists(identity _)
  def all(a : Seq[Boolean]) : Boolean = !a.exists(!_)
  def randvec(min : Double, max : Double, n : Int, r : Random) : IndexedSeq[Double] =(0 to n).map( _ => r.nextDouble * (max - min) + min) 
  
  def test_profile(blds : Vector[Bld], r : Random) : Boolean = {
    val pv = blds.map(toProfile)
    val p = skyline(blds)
    val l_ = locs(blds)
    val l = l_ ++ randvec(l_.min, l_.max, 30, r)
    val v1 = l.map(vecbld_value(blds))
    val v2 = l.map(prof_value(p))
    all((v1, v2).zipped.map( _ == _)) 
  }
    
  def test_profiles(n : Int, r : Random) : Boolean = all((0 to n).map(i => test_profile(rand_blds(5, r), r)))
  def locs( bs : Vector[Bld]) : Vector[Double] = bs.foldLeft(Vector[Double]())( (v, b : Bld) => (v :+ b.l) :+ b.r) 
  def prof_value(p : Profile)( x : Double) : Double = p.foldLeft(0.0)((vl, l : Line) => if ( x >= l.x) 
                                                                                      l.h
                                                                                     else 
                                                                                      vl)
  def bld_value(b : Bld)(x : Double) : Double = if (x >= b.l && x < b.r) 
                                                  b.h
                                                else 
                                                  0.0
  def vecbld_value(bs : Vector[Bld])(x : Double) : Double = bs.foldLeft(0.0)( (mv, b : Bld) => math.max(mv, bld_value(b)(x)))
}

sealed abstract class Bit 
case object One extends Bit 
case object Zero extends Bit

sealed abstract class Carry 
case object Gen extends Carry 
case object Prop extends Carry
case object Stop extends Carry

type BBNum = Seq[Bit]  
  

object big { 
  def toBBNum(a : Int) : BBNum = {
    if (a == 0)
      Seq[Bit]()
    else 
      ((a%2) match {
        case 0 => Zero
        case 1 => One
      })+:toBBNum(a/2)
  }
  def toInt(a : BBNum) : Int = {
    def up(t : (Int, Int), b : Bit ): (Int, Int)= {
      val (v, s) = t
      val f = b match {
        case Zero => 0
        case One  => s 
      }
      (v + f, s * 2 )
    }
    a.foldLeft((0,1))(up)._1
  }
  
  def toCarry(t : (Bit, Bit)) : Carry = 
    t match {
      case (Zero,Zero) => Stop 
      case (One, One) => Gen 
      case (_   ,_   ) => Prop
    }
  def toBit(t : (Carry, Carry)) : Bit =
    t match {
      case (Gen ,Prop) => Zero 
      case (Gen ,_   ) => One 
      case (_   ,Prop) => One 
      case (_   ,_   ) => Zero 

    }
  def prop(c0 : Carry, c1 : Carry) : Carry = 
    c1 match {
      case Prop => c0
      case _    => c1
    }

 def assoc(t : (Carry, Carry, Carry)) : Boolean = {
    val (a, b,c) = t
    prop(prop(a,b),c) == prop(a, prop(b,c))
  }
  def gentup[A]( v : Seq[A]) : Seq[(A,A,A)] =   
  {
    var l = Seq[(A,A,A)]()
    for (a <- v) 
      for (b <- v) 
        for (c <- v) 
          l = l:+(a,b,c)
    l
  }
  val no_assoc = gentup(List(Stop, Gen, Prop)).find( t => !assoc(t))
  
  def add(b0 : BBNum, b1 : BBNum) : BBNum = { 
    val cs : Seq[Carry] = b0.zipAll(b1, Zero,Zero).map( toCarry) 
    val carries = cs.scan(Stop)(prop)
    val b = (carries, cs).zipped.toList.map(toBit)  
    carries.last match {
      case Gen => b :+ One
      case _   => b
    }
  }   

  def testAdd(a : Int, b : Int) : Boolean = (a + b) == toInt(add(toBBNum(a),toBBNum(b)))
}

