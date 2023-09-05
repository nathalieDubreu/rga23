# rga23

## Suivi de la collecte
- `comptages.R` : création d'un fichier md à destination de la Dag (comptages par statut de la collecte, par éligibilité, par île, ...)

## Mises en forme des données (à lancer dans l'ordre)
- `imputationVariables.R` : impute les valeurs pour certaines variables *(AbeillesBio, PartPlantsAutoP
PartRevenusAgriExpl, PartSemencesAutoP)* en fonction des valeurs d'autres variables 
- `rostersSurfaces.R` : 
    - récupération des 7 rosters de surfaces et regroupement en 1 seul fichier 
    - imputation de valeurs pour SurfaceBio et SurfaceIrriguee (en fonction de AgriBio et Irrigation)
- `decoupageRga23.R` : découpage du fichier rga23 global en 7 sous fichiers (rga23_coprahculteurs, rga23_exploitations, rga23_general, rga23_mainOeuvre, rga23_prodAnimales, rga23_prodVegetales et rga23_tape)
- `rostersCommercialisation.R` : récupération des valeurs présentes dans les rosters de commercialisation dans les fichiers rga23_prodAnimales et rga23_prodVegetales
- `rosterMainOeuvre.R` : TODO

## Analyse

Ebauches des stats sur les différents profils.
### Non éligibles
- `nonEligibles.R`
### Eligibles
- `eligibles.R`
- `cultivateurs.R`
- `eleveurs.R`
- `coprahculteurs.R`
