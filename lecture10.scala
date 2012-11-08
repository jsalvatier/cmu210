

/* 
the BFS psuedocode always satisfies BFS' input requirements 
visited. X_i = { v in V_g | dist(s,v) < i}
and
frontier. F_i = { v in V_g | dist(s,v) ==i} 

by induction on i

case i = 0 
  BFS({}, {s}, 0) 
  let v in V_g. assume dist(s,v) < 0, but dist(s,v) >=0. contradiction
    X_0 subset {} 
    X_0 == {} 
  so X_0 is the visited nodes 
  let v in V_g. assume dist(s,v) == 0, then v ==s. s in F so v in F. 
    F_0 subset {s}
  let v in F. v == s
    so dist(s,v) == 0.
    v in F_0
    {s} subset F_0
  F_0 == {s}
case i in N 
  BFS'(X', F', i+1)
  assume X == X_i and F == F_i
  
  v in X_i+1. 
  <->dist(s,v) < i+1 <= i
  <->dist(s,v) < i or dist(s,v) == i
  <->(v in X) or (v in F)
  <->v in X union F
  <->v in X' 
  X_i+1 == X'

  let v in F'. 
  <-> v in N but not in X' 
  ... v in N_g(F)
      exists a in F. v in N_g(a)
      dist(s, a) == i 
      dist(v,a) == 1 
      so dist(s,v) <= i + 1
      v not in X'
      v not in X_i+1
      not dist(s,v) < i + 1
      dist(s,v) >= i + 1
      so dist(s,v) == i + 1
      v in F_i+1
  F_i+1 == F'
  QED
  
    
  let v in F_i+1
    dist(s,v) == i+1

  

