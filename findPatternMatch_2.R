#prüft ob zwei Matrizen gleich sind
matequal <- function(x, y)
  is.matrix(x) && is.matrix(y) && dim(x) == dim(y) && all(x == y)
  
#findPatternMatch_2 from Leila
findPatternMatch_2<-function(mask,M,col)
{
  maskSize<-dim(mask)
  nrM<-nrow(M)
  ncM<-ncol(M)
  for(i in 1:(nrM-maskSize[1]))
  {
    for(j in 1:(ncM-maskSize[1]))
    {
    N<-M[i:(i+(maskSize[1]-1)),j:(j+(maskSize[1]-1))]
    if(matequal(mask,N))
    {
      N[which(N == 1)] <- col
      M[i:(i+(maskSize[1]-1)),j:(j+(maskSize[1]-1))]<-N
    }
    }
  }
  return(M)
  }
