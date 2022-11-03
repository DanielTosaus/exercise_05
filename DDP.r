
library(combinat)
SingleDDP <- function(dX_A, dX_B, dX_AB){
  
  # Lengths of the sequences
  LA <- length(dX_A)
  LB <- length(dX_B)
  
  # Map of positions
  mapA = c(0)
  for(i in (1:LA)){
    mapA = c(mapA, (dX_A[i]+mapA[i]))
  }
  mapB = c(0)
  for(i in (1:LB)){
    mapB = c(mapB, (dX_B[i]+mapB[i]))
  }
  
  # Interleaving
  # We just check for the repeated values in the shorter vector
  if(LA > LB){
    
    interleaving <- mapA
    for(i in (1:LB)){
      # If the Map position is not in the interleaved version we add it
      if(!is.element(mapB[i], interleaving)){
        
        interleaving <- c(interleaving, mapB[i])
        
      }
    }
  }else{
    interleaving <- mapB
    for(i in (1:LA)){
      # If the Map position is not in the interleaved version we add it
      if(!is.element(mapA[i], interleaving)){
        interleaving <- c(interleaving, mapA[i])
      }
    }
    
  }
  # Sort interleaved vector
  interleaving <- sort(interleaving)
  
  # Difference between consecutive interleaved positions
  differences <- diff(interleaving)
  # We sort the differences and compare with the input set dX_AB
  sort_diff <- sort(differences)
  
  # If all the elements in sort_diff and dX_AB are the same we got a match
  if(sum(sort_diff == dX_AB) == (length(sort_diff))){
    #print("Solution found")
    #print(mapA[2:LA])
    #print(mapB[2:LB])
    return(c(mapA[2:LA], mapB[2:LB]))
  }else{
    print("Solution not found")
    return(-1)
  }
}
dX_A <- c(2, 3, 10, 5)
dX_B <- c(3, 7, 10)
dX_AB <- c(1, 2, 2, 5, 5, 5)

DDP <- function(dX_A, dX_B, dX_AB){
  # We calculate and store all the possible permutations of dX_A and dX_B
  A_perms <- permn(dX_A)
  B_perms <- permn(dX_B)
  # We calculate their lengths
  LA <- length(A_perms)
  LB <- length(B_perms)
  
  # For each possible pair of permutation we will call SingleDPP until it finds a solution
  for(i in 1:LA){
    
    A <- A_perms[[i]]
    for(j in 1:LB){
      B <- B_perms[[j]]
      #print(B)
      res <- SingleDDP(A,B, dX_AB)
      # If any of the values mismatch the input dX_AB  is not valid (keep trying)
      if(sum(res != -1) > 0){
        XA <- res[1:(length(dX_A)-1)]
        XB <- res[(length(dX_A)):(length(res))]
        print(XA)
        print(XB)
        return(res)
      }
    }
  }
  
  
}

solution <- DDP(dX_A, dX_B, dX_AB)

XA <- solution[1:(length(dX_A)-1)]
XB <- solution[(length(dX_A)):(length(solution))]







