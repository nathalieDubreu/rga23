library("dplyr")

source("fonctions.R")

# TODO - mettre à jour les variables
dossier <- "Export_02-01"
date <- "01/02/2024"
nbExploitations <- 7832

# Mises en forme des données (à lancer dans l'ordre)
source("miseEnForme/imputationVariables.R")
source("miseEnForme/ajoutsDonneesX.R")
source("miseEnForme/modificationsIdDoublonsEtX.R")
source("miseEnForme/decoupageRga23.R")
source("miseEnForme/rostersSurfaces.R")
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
source("analyse/stats.R")
source("analyse/publicationDoubleTimbre.R")

# Suivi de la collecte

## Mise à jour du fichier Rmd
# source("suiviCollecte/comptages.R")

## Contrôles en cours de collecte
### Eligibles or not
# source("suiviCollecte/nonEligibles.R")
# source("suiviCollecte/eligibles.R")
### Divers profils
# source("suiviCollecte/cultivateurs.R")
# source("suiviCollecte/eleveurs.R")
# source("suiviCollecte/coprahculteurs.R")

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
source("divers/surfacesTresPetitesAutoconsommees.R")

### Préparation fichiers tests pour le SIA
# source("divers/vivriersPilotes.R")




