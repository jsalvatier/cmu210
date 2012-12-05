import FImage._
import math._

object SeamCarve { 

  type Gradient = Double

  //task 2.1
  def generateGradients(i : FImage) : Seq[Seq[Gradient]] = { 
    def grad(x : Int, y : Int) : Gradient = {
      val c = i.data(y)(x)
      sqrt(
        pixelDifference(c, i.data(y + 1)(x)) + 
        pixelDifference(c, i.data(y)(x + 1)) 
      )
    }  
    Vector.tabulate(i.h - 1)(y => 
    Vector.tabulate(i.w - 1)(x => 
      grad(x,y)
      ))
  }

  def pixelDifference(a : Pixel, b : Pixel) : Double = 
    pow(b.r - a.r, 2) + pow(b.b - a.b, 2) + pow(b.g - a.g,2)

  /* 
  task 2.2
      j --> 
    i   0   1   2   3
    | 0 5   3   2   4
    V 1 9   3   9   10
      2 5   10  4   11
      3 9   11  10  12
      
  task 2.3
    the path 
    0,2
    1,1
    2,0
    3,0

  task 2.4

    the minimum for each index is 
    m(i,j) = g(i,j) + min(min(m(i-1,j-1), m(i-1, j)), m(i-1, j+1))
    assuming m(i,j) = inf if m is out of bounds
    then the min path cost is min_j m(h -1,j)

    to calculate the min path, calculate the min costs and then trace back
    the min cost path.

    work is O(n*m)
    span is O(n)

   */


  def findSeam(i : FImage) : Seq[Int] = { 
    val gs = generateGradients(i)
    def findM(lastM : Seq[(Double, Int)], g : Seq[Gradient]) : Seq[(Double, Int)] = { 
      def findMin(x : Int) : (Double, Int) = {
        val (m, i_min) = (
                           (x-1 to x+1) map (i => (get(g)(i) getOrElse Double.PositiveInfinity, i))
                         ).min
        (g(x) + m, i_min) 
      } 
      Vector.tabulate(i.w - 1)(findMin)
    }

    def get[A](v : Seq[A])(i : Int) : Option[A] = 
      try 
        Some(v(i))
      catch { 
        case _: IndexOutOfBoundsException => None
      }

    def argMin[A](s : Seq[A])(implicit o : Ordering[A]) : Int = 
      s.zipWithIndex.min._2

    val zero = Seq.fill(i.w-1)((0.0,0))
    val m = gs.scanLeft(zero)(findM)  
  
    val minstart = m(i.h -1).min
    val s = m.scanRight(minstart._2)(
      (mrow,min_i) => mrow(min_i)._2
    )
    s.slice(1,s.size)
  }


  def removeSeam(i : FImage) : FImage = {
    val s = findSeam(i)
    val ndata = (s, i.data).zipped.map( (j, r) => 
        Vector.tabulate(i.w -1)(i => if (i < j) r(i) else r(i+1) )
        )
    FImage.FImage(i.w -1, i.h, ndata)
  }
  def removeSeams(i : FImage)(n : Int) : FImage = 
    (0 to n).foldLeft(i)( (ni,_) => removeSeam(ni)) 
  



}
