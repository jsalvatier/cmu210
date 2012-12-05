import AllShortestPaths._
import scala.util._

case class ASPTest( aspl : All_Shortest_Paths) {
  import aspl._

  def genGraph(r : Random, nv : Int, ne : Int) : Graph = 
    makeGraph((0 to ne).map(_ => r.nextInt(nv) -> r.nextInt(nv))) 

  /*
  def testAsp(s : Int) { 
    val r = new Random(s)
   (5 to 20).map( 
  }
  */
}

case object ASPDebug { 
  import AllShortestPaths.ASPVectorMap._
  def genGraph(r : Random, nv : Int, ne : Int) : Graph = 
    makeGraph((0 to ne).map(_ => r.nextInt(nv) -> r.nextInt(nv))) 


}
