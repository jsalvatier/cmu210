import AllShortestPaths._ 

case class ThesaurusASP( asp : All_Shortest_Paths[String]) { 
  import asp._

  type Thesaurus = Graph 
  def make(t : Seq[(String, Seq[String])] ) : Thesaurus  =  
    makeGraph(
      t.flatMap( t => {
        val (w, syns) = t
        syns.map( (_,w) )
      }
      )
    )
  def numWords( t : Thesaurus) : Int = numEdges(t)
  def synonyms( t : Thesaurus)(w : String) : Seq[String] = outNeighbors(t)(w)

  def query(t : Thesaurus)(w : String): String=>Seq[Seq[String]]= 
    report(makeASP(t)(w))
    
}
