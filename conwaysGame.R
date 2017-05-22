getNeighbours <- function(M, i, j) {
  # Selina
  # TODO: finde Nachbarn von M(i,j)
  # Beachte den Randfall: bsp. für linke obere Ecke:
  # Nachbar oben: von unten
  # Nachbar links: von rechts
  # Nachbar oben links: von rechts unten
  
  # return 3x3 Matrix (M(i,j) in der Mitte und seine Nachbarn)
}

computeIsAlive <- function(N) {
  # Leila
  # TODO: 
  # gegeben 3x3 Nachbarmatrix für ein Wert
  # Regeln des Spiels implementieren
  # return 0(Tot) oder 1(Lebend)
}


computeAll <- function(M, save_path) {
  # Selina
  # TODO: iteriere über alle MAtrix elemente
  # berechne die Nachbar-Matrix N = getNeighbours(M, i, j)
  # rufe computeIsAlive(N) auf
  # rufe M(i,j)=computeIsAlive(N) auf ???
}

visualise <- function(M) {
  # Ana
  # TODO: die Matrix als Plot in R anzeigen
  # achte auf korrekte Orientierung
  # bzw. Farbwerte
}


save <- function(M, save_path, iter_id) {
  # Ana
  # TODO:
  # speichere aktuelle Matrix als .png? unter save_path
  # in dem Namen Iterationszahl
}

createMatrix <- function(matrix_size) {
  # Leila
  # TODO:
  # eine quadr. Matrix von gegebener Größe erstellen
  # Matrix mit zufälligen Werten (0 oder 1) füllen
  # return M (matrix_size x matrix_size)
}


starteSpiel <- function(iter_number, matrix_size, save_path) {
  M <- createMatrix(matrix_size)
  for (i in c(1:iter_number)) { # check if for-Loop correct
    computeAll(M)
    visualise(M)
    save(M, save_path, i)
  }
}

# Spiel starten: 
# starteSpiel(1000, 300, '..blabla');
