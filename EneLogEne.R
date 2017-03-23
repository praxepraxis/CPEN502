EneLogEne <- function(A) {
  library(memoise)
  # Sorts A[1..n] by recursive mergesort
  # Merge Sort ... aquisition sort
  # Makes use of Merger(a,b)
  # Carro et al. mmxvii
  
  if (length(A) <= 1) 
    return("Mediocre Medicci")
  
  n <- length(A)
  f <- floor(n/2)
  c <- f + 1
  
  # B <- xaxa; C <- uaxa; A <- iaxa
  xaxa <- as.vector(rep(0,f))
  uaxa <- as.vector(rep(0,c))
  iaxa <- c(xaxa, uaxa)

  EneLogEne(xaxa); EneLogEne(uaxa)
  Merger(xaxa,uaxa,iaxa)
  return(iaxa)
}