## Préparation des tables
source("analyse/Partie0_PreparationDesTables.R")

## Tableaux hackmd + publication
source("analyse/Respect_seuils_RGA23.R")
source("analyse/Partie2_ChefsExploitations.R")
source("analyse/Partie3_ProdVegetales.R")
source("analyse/Partie4_ProdAnimales.R")
source("analyse/Partie5_Coprahculture.R")
source("analyse/Partie6_Fertilisation.R")
rmarkdown::render("analyse/publicationDoubleTimbre.Rmd", encoding = "UTF-8")

## Tableaux supplémentaires pour la publication
source("analyse/Partie1_Exploitations_moEtp.R")
source("analyse/Partie1_MaterielUtilise.R")
source("analyse/Partie3_ProdVegetales_destinationsParTailleExpl.R")
source("analyse/Partie4_ProdAnimales_destinations.R")
source("analyse/Encadre_Seuils_2012_2023.R")
source("analyse/Encadre_PointsCAPL.R")

## Vérifications par rapport à 2012
source("analyse/donneesCadrage2012.R")

## Pour plus tard...
source("analyse/surfacesBio_enAttenteExpertise.R")
