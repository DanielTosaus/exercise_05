

PDP <- function(L){
  
  w <- max(L)
  print(max)
  L <- L[! L %in% w]
  X <- c(0, w)
  Place(L, X)
}

Place <- function(L, X){
  
  if(lengt(L) == 0){
    return(X)
  }
  y <- max(L)
  gen_fragements <- delta(y,X)
  if(sum(is.element(gen_fragements, X)) == length(gen_fragements)){
    
  }
  
}

delta <- function(y, X){
  
  return(abs(X-y))
}
L <- c(1, 2)
PDP(L)

