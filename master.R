library("dplyr")

source("fonctions.R")

# TODO - mettre à jour le nom du dossier contenant l'export dézippé de SuSo
dossier <- "Export_08-23"

# Mises en forme des données
source("miseEnForme/rostersSurfaces.R")
source("miseEnForme/decoupageRga23.R")
#source("miseEnForme/rostersCommercialisation.R")

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
source("analyse/coprahculteurs.R")




