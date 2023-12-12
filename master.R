library("dplyr")
# library("haven")

source("fonctions.R")

# TODO - mettre à jour le nom du dossier contenant l'export dézippé de SuSo
dossier <- "Export_11-30"
date <- "30/11/2023"

# Suivi de la collecte
## Mise à jour du fichier Rmd
nbExploitations <- 7832
source("suiviCollecte/comptages.R")

# Mises en forme des données (à lancer dans l'ordre)
source("miseEnForme/imputationVariables.R")
source("miseEnForme/modificationsIdDoublonsEtX.R")
source("miseEnForme/rostersSurfaces.R")
source("miseEnForme/decoupageRga23.R")
source("miseEnForme/rostersCommercialisation.R")
source("miseEnForme/rosterCocoteraies.R")
source("miseEnForme/rostersMainOeuvre.R")
source("miseEnForme/rostersParcellesEtSites.R")
source("miseEnForme/rosterEngraisOrganiques.R")

# Champs (RGA23, RGA12, seuils CAPL)
source("champs/champRGA.R")
source("champs/champ2012.R")
source("champs/champCAPL.R")
source("champs/indicatricesAppartenances.R")

# Analyse
## Non éligibles
source("analyse/nonEligibles.R")

## Eligibles
source("analyse/eligibles.R")
## Divers profils
source("analyse/cultivateurs.R")
source("analyse/eleveurs.R")
source("analyse/coprahculteurs.R")
source("analyse/stats.R")

# Qualité 

## ONE SHOT : 
### Ajout locataires lotissements agricoles
# source("divers/lotissementsAgricoles.R")
# source("divers/attributaires.R")
### Injoignables (pour Jérôme)
# source("divers/injoignables.R")
## Préparation fichiers tests pour le SIA
# source("divers/fichiersTestSIA.R")

## Gestion des doublons
source("divers/doublons.R")
## Repérage des valeurs extremes (variables quantitatives)
source("divers/valeursExtremes.R")




