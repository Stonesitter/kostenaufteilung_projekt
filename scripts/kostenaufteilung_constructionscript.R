# erstes Beispiel um notwendigkeit der Funktion zu verstehen

# rm(list = ls())
# erstellen der participants names
# participants <- scan(what = "")

# Funktion erstellen die eine Matrix/Dataframe erstellt für alle Participants
# die dabei sind. 

# Welche Bedingungen braucht es?
# 1. Muss GesamtKosten und einzelne Ausgaben einer Person darstellen
# 2. Muss angeben welche MitParticipants bei einzelnen Kosten dabei waren
#
# manu Beispiel

participants <- c("manu", "kira", "jami", "almut", "anthea")

# erstellen der ausgaben list
manu.ausgabe <- list(list(86.92, c(TRUE, TRUE, TRUE, TRUE, FALSE)))
kira.ausgabe <- list(list(27.50, c(TRUE, TRUE, TRUE, FALSE, FALSE)))
jami.ausgabe <- list(list(25, c(TRUE, TRUE, TRUE, FALSE, FALSE)))
almut.ausgabe <- list(list(0, c(TRUE, TRUE, TRUE, TRUE, FALSE)))
anthea.ausgabe <- list(list(47.50, c(TRUE, TRUE, TRUE, FALSE, FALSE)))



######## Ideen
# 1. eine get.participants() funktion die mal die namen der Participants abfragt

# 2. eine neuer.eintrag() funktion. die abfragt für wen die ausgabe ist, wie
# hoch sie war und für wen alle gezahlt wurde. bevor ich das tue wäre aber noch
# das endformat interessant, die Liste scheint momentan ungut




# zu.zahlen.matrix erstellen 
matrix <- matrix(ncol = length(participants))
matrix[, ] <- 0
colnames(matrix) <- participants
matrix


## # einzelne wertebestimmen
## zahlen <- sapply(kira.ausgabe, "[[", 1)
## sum(zahlen)

## anz <- sapply(kira.ausgabe, "[[", 2)
## teil.factor <- colSums(anz)
## end.wert <- zahlen / teil.factor
## teil.factor <- sum(kira.ausgabe[[1]][[2]])
## zahlen




## x <- vector(end.wert * kira.ausgabe[[1]][[2]])
## temp.m <- t(anz)
## temp.m
## colnames(temp.m) <- participants
## rownames(temp.m) <- zahlen
## temp.m temp.m * end.wert


## algemein formulieren

# hilfsfunktion die aus den einzelnen ausgaben einer person eine pers.matrix
# erstellt

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



## # scan funktion die die namen der participants erstellt

## get.participants <- function() {
##     cat("Bitte uebergebe alle Teilnehmer.\n Wenn du fertig bist uebergebe ein leeres Objekt\n")
##     participants <- scan(what = character())
##     participants <- participants
##     return(participants <- participants)
## }

## ### ausgaben listen mit scan erstellen
## for (i in participants) {

##     {
##     x <- numeric(0)
##     while(bool = TRUE)
##     t <- scan(nmax = 1)
##     x <- c(t, x)
##     z <- scan(nmax = length(participants))  
##     z <- as.logical(z)
## }




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

colSums(matrix)


ausgaben.summe <- rep(NA, times = length(participants))
for (i in 1:length(participants)) {
    ausgaben.summe[i] <- sum(sapply(eval(temp[i]), "[[", 1))
    ## anz <- rep(NA, times = length(participants))
    ## anz[i] <- sapply(eval(temp[i]), "[[", 2)
}

sollwert <- ausgaben.summe - colSums(matrix)
sign(sollwert)



sollwert
glaeubiger <- numeric()
schuldiger <- numeric()

for (i in 1:length(sollwert)) {
    if (sollwert[i] >= 0) {
        glaeubiger <- c(glaeubiger, sollwert[i])
    } else {
        schuldiger <- c(schuldiger, sollwert[i])
    }
}
        
# Beispielwerte

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
        
