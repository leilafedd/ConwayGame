computeAll <- function(M) {
  # Selina
  # TODO: iteriere über alle MAtrix elemente
  # berechne die Nachbar-Matrix N = getNeighbours(M, i, j)
  # rufe computeIsAlive(N) auf
  # rufe M(i,j)=computeIsAlive(N) auf
  
  # save_path???
  
  #a<-apply(N, MARGIN=c(1,2), function(x) x + 2)
  ncM <- ncol(M)
  nrM <- nrow(M)
  
  for (i in 1:ncM){
    for(j in 1:nrM){
      N<-getNeighbours(M,i,j)
      M[i,j] <- computeIsAlive(N)
    }
  }
  return(M)
}
