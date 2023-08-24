library("dplyr")

source("fonctions.R")

# TODO - mettre à jour
dossier <- "Export_08-24"

# Mises en forme des données
source("miseEnForme/rostersSurfaces.R")
source("miseEnForme/rostersCommercialisation.R")
# source("miseEnForme/decoupageRga23.R")

# Suivi de la collecte
## Mise à jour du fichier Rmd
#date <- "24/08/2023"
#source("suiviCollecte/comptages.R")

# Analyse 
## Non éligibles
source("analyse/nonEligibles.R")
## Eligibles
source("analyse/eligibles.R")
source("analyse/cultivateurs.R")
source("analyse/eleveurs.R")




