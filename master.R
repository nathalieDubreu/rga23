library("dplyr")
library("rmarkdown")
library("knitr")
library("tidyr")

source("fonctions.R")

# Champs (RGA23, RGA12, seuils CAPL)
source("champs/champRGA.R")
source("champs/champ2012.R")
source("champs/champCAPL.R")
source("champs/comparatifsAppartenances.R")
## Tests unitaires
source("champs/champCAPL_tests.R")

# Analyse
source("analyse/publicationDoubleTimbre.R")

# Tape
source("tape/tape.R")


######## Programmes des étapes préalables à l'analyse

# TODO - mettre à jour les variables pour le suivi de la collecte et/ou la mise en forme des données et/ou certains programmes de la catégorie Divers
dossier <- "Export_02-28"
date <- "28/02/2024"
nbExploitations <- 7832

# Mises en forme des données (à lancer dans l'ordre)
source("miseEnForme/interviewKeysAExclure.R")
source("miseEnForme/rosterEngraisOrganiques.R")
source("miseEnForme/imputationVariables.R")
source("miseEnForme/correctionsAlimentationAnimaux.R")
source("miseEnForme/correctionsValorisationEngrais.R")
source("miseEnForme/ajoutsDonneesX.R")
source("miseEnForme/ajoutVariableBio.R")
source("miseEnForme/modificationsIdDoublonsEtX.R")
source("miseEnForme/decoupageRga23.R")
source("miseEnForme/rostersSurfaces.R")
source("miseEnForme/rostersCommercialisation.R")
source("miseEnForme/rosterCocoteraies.R")
source("miseEnForme/rostersMainOeuvre.R")
source("miseEnForme/rostersParcellesEtSites.R")
source("miseEnForme/ajoutIndicatrices.R")
## Selon la destination :
#### Pour mise à dispo générale de la base de données :
source("miseEnForme/anonymisation.R")
source("miseEnForme/modificationsFichiersBaseStat.R")
#### Pour intégration dans le SIA :
# source("miseEnForme/integrationSIA.R")

# Suivi de la collecte

## Mise à jour des fichiers Rmd 
source("suiviCollecte/comptages.R")
source("suiviCollecte/stats.R")

## Contrôles en cours de collecte
### Eligibles or not
source("suiviCollecte/nonEligibles.R")
source("suiviCollecte/eligibles.R")
### Divers profils
source("suiviCollecte/cultivateurs.R")
source("suiviCollecte/eleveurs.R")
source("suiviCollecte/coprahculteurs.R")

# Divers (qualités, extractions, corrections.... - programmes le plus souvent one shot) 

### Ajout locataires lotissements agricoles
source("divers/lotissementsAgricoles.R")
source("divers/attributaires.R")

### Injoignables (pour Jérôme)
source("divers/injoignables.R")

## Préparation fichiers tests pour le SIA
source("divers/fichiersTestSIA.R")

### Modifications des identifiants dans la base (doublons et X devenus P ou C)
source("divers/modificationsIdBase.R")

### Gestion des doublons
source("divers/doublons.R")
 
## Apurement
### Repérage des valeurs extremes (variables quantitatives)
source("divers/valeursExtremes.R")
### Questions métier - cf. trello
source("divers/questionsMetierApurement.R")
source("divers/surfacesTresPetitesAutoconsommees.R")

### Préparation fichiers pour l'enquête complémentaire sur le vivrier
source("divers/vivriersPilotes.R")


