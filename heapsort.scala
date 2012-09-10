




class heap[T](A : Array[T])(implicit o: Ordering[T]){ 
	val size = A.length
  
	for (i <- (A.length/2 -1) to 0 by -1)
	  maxheapify(i)
  
	override def toString() = "heap : " ++ A.mkString(",")
	def left (i : Int) = 2*i
	def right (i : Int) = 2*i + 1
	def parent (i : Int) = i/2
	
	def maxheapify(i : Int) : Unit = {
	  val v = A(i)
	  var l = left(i)
	  var r = right(i)
	  var largest = i
	    
	  if (l < A.length && o.gt(A(l), A(largest))) 
		  largest = l
	  if (r < A.length && o.gt(A(r), A(largest))) 
	  	  largest = r
	  	  
	  if (largest != i) { 
	    A(i) = A(largest)
	    A(largest) = v
	    maxheapify(largest)
	  }
	}
	
}



object sorting{
	
  def quicksort[T](a : Array[T])( implicit o : Ordering[T]){  
    def swap(x : Int, y : Int) { 
      val b = a(x)
      a(x) = a(y) 
      a(y) = b
    }
        
    def partition(start : Int, end : Int) : Int= {
	    var i = start
	    val p = a(end -1)
	   
	    for (j <- start until end -1 ) {
	      if (o.lt(a(j), p)){
	        swap(i, j)
	        i += 1
	      }
	    }
	    swap(i, end -1)
	    return i
	} 
    def qsort(start : Int, end : Int) {
	  if (end - start > 1){
        val q = partition(start, end)
		
		qsort(start, q)
		qsort(q+1, end)
	  }
	
	}
    qsort(0, a.length)
  }
  
  def countingsort( a : Array[Int], k : Int) : Array[Int] = {
    var counts = Array.fill(k)(0)
    for (i <- 0 until a.length){
      counts(a(i)) += 1 
    }
    
    var scounts = counts.scanLeft(0)(_+_)
    var b = Array.fill(k)(0)
     
    for (i <- a.length to 1 by -1){
      Console.println(a(i-1))
      Console.println(scounts(a(i - 1)))
      Console.println("")
      b(scounts(a(i - 1)))= a(i - 1)
      scounts(a(i - 1)) -= 1    

    }
    return b
  }
  
}


object heapsort {
	def main(args: Array[String]) {
	    val a = Array(1,3,5,-7)
	    
            val h = new heap(a)
		
	    val b = Array(1,3,5,-7, 11)
	    sorting.quicksort(b)
	    sorting.countingsort(Array(5,4,4,4,5,1,2), 6) 
	}	

}
