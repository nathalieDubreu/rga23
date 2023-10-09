library("dplyr")

source("fonctions.R")

# TODO - mettre à jour le nom du dossier contenant l'export dézippé de SuSo
dossier <- "Export_10-09"

# Suivi de la collecte
## Mise à jour du fichier Rmd
date <- "09/10/2023"
source("suiviCollecte/comptages.R")

# Mises en forme des données (à lancer dans l'ordre)
source("miseEnForme/imputationVariables.R")
source("miseEnForme/rostersSurfaces.R")
source("miseEnForme/decoupageRga23.R")
source("miseEnForme/rostersCommercialisation.R")
source("miseEnForme/rosterCocoteraies.R")
source("miseEnForme/rostersMainOeuvre.R")
source("miseEnForme/rostersParcellesEtSites.R")
source("miseEnForme/rosterEngraisOrganiques.R")

# Analyse
## Non éligibles
source("analyse/nonEligibles.R")
## Eligibles
source("analyse/eligibles.R")
## Dans le champ du RGA23
source("analyse/champRGA.R")
## Divers profils
source("analyse/cultivateurs.R")
source("analyse/eleveurs.R")
source("analyse/coprahculteurs.R")
source("analyse/stats.R")

# Qualité - ajout locataires lotissements agricoles
# source("qualite/lotissementsAgricoles.R")
# source("qualite/attributaires.R")




