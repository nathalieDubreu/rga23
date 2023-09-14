# rga23

## Suivi de la collecte
- `comptages.R` : création d'un fichier md à destination de la Dag (comptages par statut de la collecte, par éligibilité, par île, ...)

## Mises en forme des données (à lancer dans l'ordre) => création des 16 fichiers de données
- `imputationVariables.R` :
    - impute les valeurs pour certaines variables *(AbeillesBio, PartPlantsAutoP, PartRevenusAgriExpl, PartSemencesAutoP, ...)* en fonction des valeurs d'autres variables
    - traitement des noms, prénoms et téléphones corrigés + passage des NSP (valeur par défaut = 1) en NA pour la SAU et la surface de végétation naturelle
- `rostersSurfaces.R` : 
    - récupération des 7 rosters de surfaces et regroupement en 1 seul fichier 
    - imputation de valeurs pour SurfaceBio et SurfaceIrriguee (en fonction de AgriBio et Irrigation)
    - création du fichier csv correspondant -> **rga23_surfacesCultures**
- `decoupageRga23.R` : découpage du fichier rga23 global en 9 sous fichiers (**rga23_coprahculteurs**, **rga23_exploitations**, **rga23_general**, rga23_mainOeuvre, rga23_prodAnimales, rga23_prodVegetales, **rga23_gestion**, **rga23_peche** et **rga23_tape**)
- `rostersCommercialisation.R` :
    - récupération des valeurs présentes dans les rosters de commercialisation dans **rga23_prodAnimales** et **rga23_prodVegetales**
    - imputation de la valeur 100 aux variables de part si une seule modalité est sélectionnée
- `rosterCocoteraies.R` :
    - imputation de la valeur 100 à PartCoco si tout le revenu est conservé
    - création du fichier CSV correspondant au roster : roster_coco_loc.tab -> **rga23_cocoteraies**
- `rostersMainOeuvre.R` :
    - récupération des valeurs du roster MONonFamPerm.tab dans **rga23_mainOeuvre**
    - création de 2 fichiers CSV correspondant aux rosters RosterCoExploit.tab et RosterMOPermFam.tab -> **rga23_coexploitants** et **rga23_moPermanenteFam**
- `rostersParcellesEtSites.R` : création de 2 fichiers CSV correspondant aux rosters roster_accesSite.tab et roster_parcelles.tab -> **rga23_sites** et **rga23_parcelles**
- `rosterEngraisOrganiques.R` : création du fichier CSV correspondant au roster : roster_engrais_orga.tab -> **rga23_engraisOrga**

## Analyse

Ebauches des stats sur les différents profils.
### Non éligibles
- `nonEligibles.R`
### Eligibles
- `eligibles.R`
- `cultivateurs.R`
- `eleveurs.R`
- `coprahculteurs.R`
