

/* 
task 3.1
  4 + 4 + 2 + 1 times 

task 3.2
  4 + 1 times

task 3.3
  C(S_i, s_j) = 
    if (S_i == s_j)
      1
    else 
      if (j > 0) 
        C(S_i, s_{j-1}) + if (i > 0) C(S_{i-1}, s_{j-1}) else 0 
      else 
        0
  C(0, s_j) = 1

task 3.4
  |S| = n
  |s| = m

  verticies in DAG == work ==_O n*m 
  longest path in DAG = span ==_O m

  each problem depends on the two with strings 1 shorter than them. thus each cycle the size is reduced by 1 (when you get to string size
    zero the problem is done). 

  at each level you extend your solution by 1 for the size of the string, and extend and not extend for the size of the sequence. 


task 3.5
  "atewood" can be split up into "ate wood", but a greedy algorithm will find 
  "at" and then not be able to find anything for "ewood"

task 3.6
    
 */
