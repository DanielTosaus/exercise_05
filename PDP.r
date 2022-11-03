

PDP <- function(L){
  
  w <- max(L)
  L <- Delete(w, L)
  X <- c(0, w)
  Place(L, X, w)
}
Place <- function(L, X, w){
  
  
  if(length(L) == 0){
    print(sort(X))
    return(X)
  }
  y <- max(L)
  gen_fragments <- delta(y,X)

  if(sum(is.element(gen_fragments, L)) == length(gen_fragments)){
    X <- c(X, y)
    L <-Delete(gen_fragments, L)
    Place(L, X, w)
    X <- Delete(y, X)
    L <- c(L, gen_fragments)
    
  }
  
  gen_frag <- delta((w-y), X)
  if(sum(is.element(gen_frag, L))== length(gen_frag)){
    X <- c(X, (w-y))
    L <- Delete(gen_frag, L)
    Place(L,X, w)
    X <- Delete((w-y), X)
    L <- c(L, gen_frag)
    
  }
  return()
}

Delete <- function(w, L){
  return(L[! L %in% w])
}
delta <- function(y, X){
  
  return(abs(X-y))
}
L <- c(5,7,8,12,15,20)

X <- PDP(L)


