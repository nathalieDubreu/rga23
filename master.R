library("dplyr")

source("fonctions.R")

# TODO - mettre à jour
dossier <- "Export_08-23"

# Mises en forme des données
source("miseEnForme/rostersSurfaces.R")
# source("miseEnForme/decoupageRga23.R")

# Suivi de la collecte
## Mise à jour du fichier Rmd
date <- "22/08/2023"
source("suiviCollecte/comptages.R")

# Analyse 
## Non éligibles
source("nonEligibles.R")
## Eligibles
source("eligibles.R")




