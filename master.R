library("dplyr")

source("fonctions.R")

# TODO - mettre à jour le nom du dossier contenant l'export dézippé de SuSo
dossier <- "Export_09-05"

# Suivi de la collecte
## Mise à jour du fichier Rmd
date <- "05/09/2023"
source("suiviCollecte/comptages.R")

# Mises en forme des données (à lancer dans l'ordre)
source("miseEnForme/imputationVariables.R")
source("miseEnForme/rostersSurfaces.R")
source("miseEnForme/decoupageRga23.R")
source("miseEnForme/rostersCommercialisation.R")
source("miseEnForme/rosterMainOeuvre.R")

# Analyse
## Non éligibles
source("analyse/nonEligibles.R")
## Eligibles
source("analyse/eligibles.R")
source("analyse/cultivateurs.R")
source("analyse/eleveurs.R")
source("analyse/coprahculteurs.R")




