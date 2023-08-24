library("dplyr")

source("fonctions.R")

# Suivi de la collecte
## Mise à jour du fichier Rmd
date <- "22/08/2023"
source("suiviCollecte/comptages.R")

# Analyse 
exportRGA <- readTable("rga23.tab", "Export_08-23")
## Non éligibles
source("nonEligibles.R")
## Eligibles
source("eligibles.R")

# Mises en forme des données
dossier <- "Export_08-23"
source("miseEnForme.R")


