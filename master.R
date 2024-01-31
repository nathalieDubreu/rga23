library("dplyr")
# library("haven")

source("fonctions.R")

# TODO - mettre à jour le nom du dossier contenant l'export dézippé de SuSo
dossier <- "Export_01-26"
date <- "26/01/2023"

# Suivi de la collecte
## Mise à jour du fichier Rmd
nbExploitations <- 7832
source("suiviCollecte/comptages.R")

# Mises en forme des données (à lancer dans l'ordre)
source("miseEnForme/imputationVariables.R")
source("miseEnForme/ajoutsDonneesX.R")
source("miseEnForme/modificationsIdDoublonsEtX.R")
source("miseEnForme/decoupageRga23.R")
source("miseEnForme/rostersSurfaces.R")
source("miseEnForme/rostersCommercialisation.R")
source("suppPetitesSurfacesAutoConsom.R")
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

# Divers (qualités, extractions, corrections.... - programmes le plus souvent one shot) 

### Ajout locataires lotissements agricoles
# source("divers/lotissementsAgricoles.R")
# source("divers/attributaires.R")

### Injoignables (pour Jérôme)
# source("divers/injoignables.R")

## Préparation fichiers tests pour le SIA
# source("divers/fichiersTestSIA.R")

### Modifications des identifiants dans la base (doublons et X devenus P ou C)
# source("divers/modificationsIdBase.R")

### Gestion des doublons
# source("divers/doublons.R")
 
## Apurement
### Repérage des valeurs extremes (variables quantitatives)
source("divers/valeursExtremes.R")
### Questions métier - cf. trello
source("divers/questionsMetierApurement.R")

### Préparation fichiers tests pour le SIA
# source("divers/vivriersPilotes.R")




