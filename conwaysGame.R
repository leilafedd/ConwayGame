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
  # Leila
  # TODO: 
  # gegeben 3x3 Nachbarmatrix für ein Wert
  # Regeln des Spiels implementieren
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
  # Selina
  # TODO: iteriere über alle MAtrix elemente
  # berechne die Nachbar-Matrix N = getNeighbours(M, i, j)
  # rufe computeIsAlive(N) auf
  # rufe M(i,j)=computeIsAlive(N) auf
  
  # save_path???
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


visualise <- function(M) {
  # install.packages(plotrix)
  library(plotrix)
  # Ana
  # TODO: die Matrix als Plot in R anzeigen
  # achte auf korrekte Orientierung
  # bzw. Farbwerte
  cellcol <- M
  cellcol[which(M == 0)] = 'white'
  cellcol[which(M == 1)] = 'black'
  
  # TODO: more colors for different patterns?
  cellcol[which(M > 1)] = 'red'
  #### weitere farben hier zu ergänzen
  color2D.matplot(M, cellcolors = cellcol, border=NA)
}

save <- function(M, save_path, iter_id) {
  # Ana
  # TODO:
  # speichere aktuelle Matrix als .png? unter save_path
  # in dem Namen Iterationszahl
  dev.copy(png, sprintf('%s/cgofl_%s.png', save_path, iter_id), width=500, height=500, units="px")
  dev.off()
}

createMatrix <- function(matrix_size) {
  # Leila
  # TODO:
  # eine quadr. Matrix von gegebener Größe erstellen
  # Matrix mit zufälligen Werten (0 oder 1) füllen
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
  
  # RETURN: creature
  # creature.patterns = eine Matrix der Größe MxMxN, M - Patterngröße, N - Anzahl Patterns
  # creature.color = #45fe00
}

getPatterns <- function(paths) {
  # TODO: Leila
  # TODO: für alle paths creatures auslesen und als vector von objekten zurückgeben
  for (i in 1:length(paths)) {
    # load.masks(paths(i))
  }
}

detectPatterns <- function(M, creatures) {
  # TODO Ana
  for (i in 1:length(creatures)) {
   # for (j in 1:length(creatures[i].patterns)) {
      #M = findPatternMatch(M, creatures[i].patterns[j], creatures[i].color);  
    }
   # }
  return(M)
}

starteSpiel <- function(iter_number, matrix_size, save_path) {
  M <- createMatrix(matrix_size)
  for (i in c(1:iter_number)) { # check if for-Loop correct
    # prüfe wer überlebt hat und update
    M <- computeAll(M)
    
    # TODO:
    # finde und färbe patterns
    M = detectPatterns(M, getPatterns())
    
    # zeige M
    visualise(M);
    Sys.sleep(1);
    
    # speichere M if nötig
    if (i%%1 == 0) {
      save(M, save_path, i)
    }
    
    # setzte M zurück (= entfärben)
    M = decolorM(M);
    
  }
}
#------Test-----
M=createMatrix(10)
visualise(M)
 #computeIsAlive(N)
M.updated <-computeAll(M)
visualise(M.updated)
#------Test-----
# Spiel starten: 
# starteSpiel(1000, 300, '..blabla');
save_path = '/home/te74zej/Dokumente/M.Sc./SS2017/Programmierung  mit R/Projekt/game_test'
starteSpiel(10, 100, save_path)
