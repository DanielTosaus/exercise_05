library("Biostrings")

Sequences <- read.DNAStringSet("seq_score.fasta", "fasta")
Indexes <- c()    # What are this indexes?
l <- 3
Score <- function(Sequences, Indexes, l)
  
a <- 1:L       # Whats the real value of a?
               # Is a list of DNA sequences?
NextLeaf <- function(a,L,k,n,l){
  
  for(i in (L:1)){
    if(a[i] < k){       # If the length of ai is less than the length of the DNA sequence
      a[i] = a[i] + 1   # we go to the next leaf?
      return(a)
    }
    a[i] <- 1
  }
  return(a)
}


BFMotifSearch <- function(DNA, t, n, l){
  
  s <-c()
  for(i in (1:length(L))){
    s <- c(s, 1)
  }
  bestScore <- Score(s, DNA, l)
  
  while(TRUE){
    
    s <- NextLeaf(s, t, n-l+1)
    if(Score(s, DNA, l) > bestScore){
      
      bestScore <- Score(s, DNA, l)
      bestMotif <- s
    }
    if(sum(s==1) == length(s)){
      return(bestMotif)
    }
  }
  
}

NextVertex <- function(a, i, L, k){
  
  if(i<L){
    a[i+1] <- 1
    return(a, i+1)
  }
  else{
    for(j in (L:1)){
      
      if(a[j] < k){
        
        a[j] <- a[j] + 1
        return(a, j)
      }
    }
  }
  return(a, 0)
}

SimpleMotifSearch <- function(DNA, t, n, l){
  
  s <- c()
  for(i in (1:length(L))){
    s <- c(s, 1)
  }
  bestScore <- 0
  i <- 1
  while(i>0){
    
    if(i<t){
      
      (s, i) <- NextVertex(s, i, t, n-l+1)
    }
    else{
      if(Score(s, DNA, l) > bestScore){
        bestScore <- Score(s, DNA, l)
        bestMotif <- s
        
      }
      (s, i) <- NextVertex(s, i, t, n-l+1)
    }
    
  }
  return(bestMotif)
}











