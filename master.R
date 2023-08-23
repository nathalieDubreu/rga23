library("dplyr")

source("fonctions.R")

# TODO : à mettre à jour !
exportRGA <- readCSV("rga23_08-22.csv")
date <- "22/08/2023"

# Suivi de la collecte
## Mise à jour du fichier Rmd
source("suiviCollecte/comptages.R")

# Analyse des non éligibles
source("nonEligibles.R")

# Analyse des éligibles
source("eligibles.R")
