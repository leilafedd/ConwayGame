getNeighbours <- function(M, i,j) {
  # return 3x3 Matrix (M(i,j) in der Mitte und seine Nachbarn
  
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

computeIsAlive2 <- function(N) {
  allElem <- sum(N)
  if (N[2,2] == 1) {
    if (allElem < 5 && allElem > 2) {
      return(1)
    } else {
      return(0)
    }
  } else {
    if (allElem == 3) {
      return(1)
    } else {
      return(0)
    }
  }
}

computeIsAlive <- function(N) {
  # return 0(Tot) oder 1(Lebend)
    alive<-0
  for ( i in 1:3 )
  {
    for(j in 1:3)
    {
      if(N[i,j]== 1) alive<-alive+1
        
    }
  }
  
  if(N[2,2]==1)
  {
    alive <- alive-1
    if(alive<2 || alive>3) {return(0)}
    else
    {
    if(alive==2 || alive==3) return(1)
    }
  }
  if(N[2,2]==0)
  {
    if(alive==3) return(1)
  }
    return(0)
}


computeAll <- function(M) {
  R <- matrix(data=0, ncol=ncol(M), nrow=nrow(M))
  
  #a<-apply(N, MARGIN=c(1,2), function(x) x + 2)
  ncM <- ncol(M)
  nrM <- nrow(M)
  
  # Wie könnte man an diese Stelle sich "apply()" nützlich machen? 
  # Brauchen etwas wie R <- apply(M, @computeIsAlive(getNeghbours(M)))
  for (i in 1:nrM){
    for(j in 1:ncM){
      N<-getNeighbours(M,i,j)
      R[i,j] <- computeIsAlive(N)
    }
  }
  return(R)
}


visualise <- function(M, colorMapping) {
  tryCatch({
    library(plotrix)
  }, error = function(e) {
    install.packages("plotrix")
    library(plotrix)
  })

  cellcol <- array(, dim=dim(M))
  for (i in 1:dim(colorMapping)[1]) {
    cellcol[which(M == colorMapping$num[i])] <- colorMapping$col[i]
  }
  color2D.matplot(M, cellcolors = cellcol, border=NA)
}

save <- function(M, save_path, iter_id) {
  dev.copy(png, sprintf('%s/cgofl_%s.png', save_path, iter_id), width=500, height=500, units="px")
  dev.off()
}

createMatrix <- function(matrix_size) {
  # return M (matrix_size x matrix_size)visualise(M)
  matrix = replicate(matrix_size,sample(c(0,1),matrix_size,replace = TRUE ,c(0.5,0.5)))
  return(matrix)
}

decolorM <- function(M) {
  M[which(M >0)] = 1;
  return(M);
}


# findPatternMatch from Ana
findPatternMatch <- function(M, pattern, colNum) {
  location = matrix(, ncol=2)
  for (i in 1:(nrow(M)-nrow(pattern)-1)) {
    for (j in 1:(ncol(M)-ncol(pattern)-1)) {
      subM = M[i:(i-1+nrow(pattern)), j:(j-1+ncol(pattern))];
      if (identical(subM,pattern)) {
        subM[which(subM == 1)] = colNum
        M[i:(i-1+nrow(pattern)), j:(j-1+ncol(pattern))] = subM;
      }
    }
  }
  return(M)
}

# TODO: findPatternMatch_2 from Leila

load.masks <- function(path) {
  # TODO Selina:
    #lese die Datei aus
    # bsp:
    # #45fe00
    # 0 0 0 0 0 1 1 0 0 1 1 0 0 0 0 0
  creature <- {}
  con <- file(path, "r")
  line.number <- 1
  while(TRUE){
    line <- readLines(con, n=1)
    if(length(line) == 0){
      break
    }
    if(line.number %% 2 == 0 ){
      creature$patterns = line
    }else{
      creature$color = line
    }
    line.number <- line.number + 1
    print(line)
  }
  close(con)
  
  # mydata = read.table(path) 
  # mydata
  
  # RETURN: creature
  # creature.patterns = eine Matrix der Größe MxMxN, M - Patterngröße, N - Anzahl Patterns
  # creature.color = #45fe00
  
  # ---------------------------- FOR TESTS -------------------------------------------------------------
  # -------------------- Remove after implementing -----------------------------------------------------
  tryCatch({
    library(abind)  
  }, error = function(e){
    install.packages("abind")
    library(abind)
  })
  
  pattern1 = array(c(0,0,0,0,0, 0,0,1,0,0, 0,0,1,0,0, 0,0,1,0,0, 0,0,0,0,0), dim=c(5,5)) 
  pattern2 = array(c(0,0,0,0,0, 0,0,0,0,0, 0,1,1,1,0, 0,0,0,0,0, 0,0,0,0,0), dim=c(5,5))
  patterns = abind(pattern1, pattern2, along=3)
  
  creature <- list(color = '#00ff00', patterns=patterns)
  # ----------------------------------------------------------------------------------------------------
  # ----------------------------------------------------------------------------------------------------
             
  return(creature)
}

path <- 'color and pattern/block.txt'
load.masks(path)

# getAllPatterns <- function(paths) {
#   # für alle paths creatures auslesen und als vector von objekten zurückgeben
#   creatures <- list()
#   for (i in 1:length(paths)) {
#     creatures[[length(creatures) +1]] <- load.masks(paths[i])
#   }
#   return(creatures);
# }

detectPatterns <- function(M, creatures, colorMapping) {
  for (i in 1:length(creatures)) {
    for (j in 1:dim(creatures[[i]]$patterns)[3]) {
      col = colorMapping$num[which(colorMapping$col == creatures[[i]]$color)];
      M = findPatternMatch(M, creatures[[i]]$patterns[,,j], col);  
    }
  }
  return(M)
}

starteSpiel <- function(iter_number, matrix_size, save_path, colorMapping) {
  M <- createMatrix(matrix_size)
  
  for (i in c(1:iter_number)) { # check if for-Loop correct
    # TODO:
    # finde und färbe patterns
    paths = c('xxx')
    M = detectPatterns(M, getAllPatterns(paths), colorMapping)
    
    # zeige M
    visualise(M, colorMapping);
    Sys.sleep(1);
    
    # speichere M if nötig
    if (i%%1 == 0) {
      save(M, save_path, i)
    }
    
    # setzte M zurück (= entfärben)
    M = decolorM(M);
    
    # prüfe wer überlebt hat und update
    M <- computeAll(M)
  }
}

# Spiel starten: 
# starteSpiel(1000, 300, '..blabla');
save_path = '/home/te74zej/Dokumente/M.Sc./SS2017/Programmierung  mit R/Projekt/game_test'
save_path = 'C:/Users/aftak/Documents/FSU/M.Sc/SS2017/Programmierung mit R/conway_snapshots'

colorMapping = data.frame(num=c(0,1,5,10), col=c('white', 'black', 'red', '#00ff00'), stringsAsFactors = FALSE)


starteSpiel(10, 100, save_path, colorMapping)
