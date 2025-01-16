# zu.zahlen.matrix erstellen 
matrix <- matrix(ncol = length(participants))
matrix[, ] <- 0
colnames(matrix) <- participants
matrix

# hilfsfunktion 
pers.m <- function(name.ausgabe) {
    strings <- ls()
    name <- strsplit(strings, split = "[.]")
    name <- name[[1]][1]




    zahlen <- sapply(name.ausgabe, "[[", 1)



    anz <- sapply(name.ausgabe, "[[", 2)
    teil.factor <- colSums(anz)
    end.wert <- zahlen / teil.factor
    temp.m <- t(anz)
    colnames(temp.m) <- participants
    rownames(temp.m) <- zahlen
    temp.m <- temp.m * end.wert

    
    return(temp.m)
}



# gesamt matrix erstellen
for (i in 1:length(participants)) {
    alle.ausgabe <- paste0(participants, ".ausgabe")
    temp <- parse(text = alle.ausgabe)
    m <- pers.m(eval(temp[i]))
    ## my_list <- list(vector("list"))
    ## my_list <- vector("list", length(participants))
    ## my_list <- lapply(my_list, FUN = vector, "list", n = 2)
    ## my_list[i] <- pers.m(eval(temp[i]))
    matrix <- rbind(matrix, m)
}


# sollwert matrix erstellen
ausgaben.summe <- rep(NA, times = length(participants))
for (i in 1:length(participants)) {
    ausgaben.summe[i] <- sum(sapply(eval(temp[i]), "[[", 1))
}

sollwert <- ausgaben.summe - colSums(matrix)



for (i in 1:length(sollwert)) {
    if (sollwert[i] >= 0) {
        glaeubiger <- c(glaeubiger, sollwert[i])
    } else {
        schuldiger <- c(schuldiger, sollwert[i])
    }
}
        

# Funktion zur Berechnung der effizientesten Rückzahlung
effiziente_rueckzahlung <- function(sollwert) {
  # Trenne Gläubiger (positive Werte) und Schuldner (negative Werte)
  glaeubiger <- sort(sollwert[sollwert > 0], decreasing = TRUE)
  schuldiger <- sort(sollwert[sollwert < 0])
  
  # Ergebnisse speichern
  transaktionen <- list()
  
  # Solange es noch Schulden oder Guthaben gibt
  while (length(glaeubiger) > 0 && length(schuldiger) > 0) {
    # Nimm den ersten Gläubiger und Schuldner
    g_name <- names(glaeubiger)[1]
    s_name <- names(schuldiger)[1]
    
    g_betrag <- glaeubiger[1]
    s_betrag <- schuldiger[1]
    
    # Bestimme die Rückzahlung (minimale Schuld oder Guthaben)
    rueckzahlung <- min(g_betrag, abs(s_betrag))
    
    # Speichere die Transaktion
    transaktionen <- append(transaktionen, list(c(von = s_name, an = g_name, betrag = rueckzahlung)))
    
    # Aktualisiere die Beträge
    glaeubiger[1] <- g_betrag - rueckzahlung
    schuldiger[1] <- s_betrag + rueckzahlung  
    # Schuld wird reduziert (Richtung 0)
    
    # Entferne ausgeglichene Einträge
    if (glaeubiger[1] == 0) glaeubiger <- glaeubiger[-1]
    if (schuldiger[1] == 0) schuldiger <- schuldiger[-1]
  }
  
  # Ergebnis zurückgeben
  return(transaktionen)
}


# Test der Funktion
transaktionen <- effiziente_rueckzahlung(sollwert)

# Ausgabe der Transaktionen
for (t in transaktionen) {
  cat(sprintf("Von: %s, An: %s, Betrag: %.2f\n", t["von"], t["an"], round(as.numeric(t["betrag"]), digits = 2)))
}
        
