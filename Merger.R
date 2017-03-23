Merger <- function(Dow,DuPont,Londini) {
  # Function of the merger of The Dow Chemical Company and E. I. du Pont de Nemours and Company
  # Merges assets of Dow and DuPont sorted pro ordem ene log ene. 
  # "(ellipsis) [A] rare merger of equals."
  #  n <- floor(n/2) + ceiling(n/2)
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  #%%%%%%%%%%% TEST VECTORS %%%%%%%%%%%%%%
  
  # D1 <- abs(rnorm(5,0,1)); b1 <- D1/min(D1)
  # D2 <- abs(rnorm(5,1,2)); b2 <- D2/min(D2)
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  # n <- length(c(Dow,DuPont))
  # A <- vector(length = n)
  
  # p <- length(Dow); q <- length(DuPont)
  # A <- as.vector(c(Dow,DuPont))
  # n <- length(A)
  
  # print("%%%%%%%%%%%%%%%%%%%%%%%%%%")
  # print(Dow); print(DuPont); print(A)
  # print("%%%%%%%%%%%%%%%%%%%%%%%%%%")
  
  # if (n %% 2 != 0) {
  #   p <- floor(n/2); q <- n - p + 1}
  # else {p <- n/2; q <- n - p}
  
  A <- Londini
  u <- length(Dow); v <- length(DuPont)
  A <- as.vector(c(Dow,DuPont))
  n <- length(A)
  if (modulo(n,2) != 0) {
    p <- floor(n/2); q <- ceiling(n/2)}
  else
    p <- n/2; q <- n-p
  
  if ((p+q)!=n)
    return("Halcyonic Caveat")
  
  # print("%%%%%%%%%%%%%%%%%%%%%%%%%%")
  # print(Dow); print(DuPont); print(A)
  # print("%%%%%%%%%%%%%%%%%%%%%%%%%%")
  # 
  # print(c(p,q))
  # print("%%%%%%%%%%%%%%%%%%%%%%%%%%")
  
  if (u < v) {
    # menlo <- c(Dow,rep(0,v-u))
    Dow <- as.vector(c(Dow,rep(0,v-u)))}
  else if (u > v) {
    # ihill <- c(rep(0,u-v),DuPont)
    DuPont <- as.vector(c(rep(0,u-v),DuPont))} # ;)
  else {
    Dow <- as.vector(Dow); DuPont <- as.vector(DuPont)}
  
  i <- 1; j <- i; k <- j
  while (i <= p && j <= q) {
    if (Dow[i] <= DuPont[j]) {
      A[k] <- Dow[i]; i <- i + 1}
    else {A[k] <- DuPont[j]; j <- j + 1}
    k <- k + 1}
  
  # Z <- vector(length = p+q-k+1) #n = p+q
  # Z <- vector(length = n-k+1)
  # Z <- as.vector(A[1:(n-k+1)])
  # print(Z)
  
  # tolua <- q-j ; icao <- p-i ; igari <- n-k
  # A <- [tolua  icao    -> igari] if n/2 == p == q;
  # A <- [icao+1 tolua-1 -> igari] if tolua < igari-icao;
  # A <- [icao-1 tolua+1 -> igari] if igari-tolua > icao. 
  
  # if (i == p && icao == tolua) 
  #   A <- as.vector(c(A[1:p],DuPont))
  # else if (j == q && icao == tolua)
  #   A <- as.vector(Dow,c(A[1:q]))
  # return(A[A!=0])
  
  Londini <- A[A!=0]
  return(Londini)
  
  # if (i == p) 
  #   A <- as.vector(c(A[k:q],DuPont))
  # else
  #   A <- as.vector(c(A[k:p],Dow))
  # # return(A[A!=0])
  # return(A)
}



