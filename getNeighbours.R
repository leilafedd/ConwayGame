getNeighbours <- function(M, i,j) {
  # Selina
  # TODO: finde Nachbarn von M(i,j)
  # Beachte den Randfall: bsp. für linke obere Ecke:
  # Nachbar oben: von unten
  # Nachbar links: von rechts
  # Nachbar oben links: von rechts unten
  
  # return 3x3 Matrix (M(i,j) in der Mitte und seine Nachbarn)
  ncM <- ncol(M)
  #nrow == ncol, da es eine quadratische Matrix ist
  #nrM <- nrow(M)
  N <- matrix(nrow = 3, ncol = 3);
  
  if(i == 1 || i == ncM || j == 1 || j == ncM ){

    a <- i-1
    if(a == 0) a <-ncM
    b <- i+1
    if(b == ncM+1) b <-1
    c <- j-1
    if(c == 0)c <-ncM
    d <- j+1
    if(d==ncM+1) d <- 1
    
    N[1,1] <- M[a,c]
    N[2,1] <- M[i,c]
    N[3,1] <- M[b,c]
    N[1,2] <- M[a,j]
    N[2,2] <- M[i,j]
    N[3,2] <- M[b,j]
    N[1,3] <- M[a,d]
    N[2,3] <- M[i,d]
    N[3,3] <- M[b,d]
 
    
  }else{
    
    e<-i-1
    f<-i+1
    g<-j-1
    h<-j+1
    N<-M[e:f,g:h]
  }

  return(N)
}