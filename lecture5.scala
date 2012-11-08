import scala.math._
import scala.util._

object lec5 { 
  case class SumSum(tot : Int, max : Int, maxl : Int, maxr : Int)
  def single(v : Int) : SumSum = SumSum(v, v, v,v)
  val empty = single(0) 
  def combine(s0 : SumSum, s1 : SumSum) : SumSum = SumSum(s0.tot + s1.tot, 
                                                          max(max(s0.max, s1.max), s0.maxr + s1.maxl), 
                                                          max(s0.maxl, s0.tot + s1.maxl), 
                                                          max(s1.maxr, s1.tot + s0.maxr))
  def mcss(s : Seq[Int]) : Int = (s.map(single).reduce(combine)).max

  def mcss_brute(s : Seq[Int]) : Int = contig_subseq(s).map(_.sum).max
  def contig_subseq(s : Seq[Int]) : Seq[Seq[Int]] = {
    if (s.nonEmpty) 
      ((0 to s.length).map(s.take) ++ contig_subseq(s.tail))
    else 
      Seq[Seq[Int]]()
  }

  def rand_seq(n : Int, r : Random) : Seq[Int] = (0 to n).map(_ => r.nextInt(20) - 10)
  def test_mcss(n : Int, r : Random): Boolean = { 
    val rs = (0 to 50).map(_ => rand_seq(n ,r))
    (rs.map(mcss), rs.map(mcss_brute)).zipped.forall(_ == _)
  }
}
