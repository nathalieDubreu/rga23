# rga23

## Suivi de la collecte
- `comptages.R` : création d'un fichier md à destination de la Dag (comptages par statut de la collecte, par éligibilité, par île, ...)

## Mises en forme des données (à lancer dans l'ordre) => création des 16 fichiers de données
- `imputationVariables.R` :
    - impute les valeurs pour certaines variables *(AbeillesBio, PartPlantsAutoP, PartRevenusAgriExpl, PartSemencesAutoP, ...)* en fonction des valeurs d'autres variables
    - traitement des noms, prénoms et téléphones corrigés
- `rostersSurfaces.R` : 
    - récupération des 7 rosters de surfaces et regroupement en 1 seul fichier 
    - imputation de valeurs pour SurfaceBio et SurfaceIrriguee (en fonction de AgriBio et Irrigation)
    - création du fichier csv correspondant -> <font color = "Blue">rga23_surfacesCultures</font>
- `decoupageRga23.R` : découpage du fichier rga23 global en 9 sous fichiers (<font color = "Blue">rga23_coprahculteurs, rga23_exploitations, rga23_general</font>, rga23_mainOeuvre, rga23_prodAnimales, rga23_prodVegetales, <font color = "Blue">rga23_gestion, rga23_peche et rga23_tape</font>)
- `rostersCommercialisation.R` :
    - récupération des valeurs présentes dans les rosters de commercialisation dans <font color = "Blue">rga23_prodAnimales et rga23_prodVegetales</font>
    - imputation de la valeur 100 aux variables de part si une seule modalité est sélectionnée
- `rosterCocoteraies.R` :
    - imputation de la valeur 100 à PartCoco si tout le revenu est conservé
    - création du fichier CSV correspondant au roster : roster_coco_loc.tab -> <font color = "Blue">rga23_cocoteraies</font>
- `rostersMainOeuvre.R` :
    - récupération des valeurs du roster MONonFamPerm.tab dans <font color = "Blue">rga23_mainOeuvre</font>
    - création de 2 fichiers CSV correspondant aux rosters RosterCoExploit.tab et RosterMOPermFam.tab -> <font color = "Blue">rga23_coexploitants et rga23_moPermanenteFam</font>
- `rostersParcellesEtSites.R` : création de 2 fichiers CSV correspondant aux rosters roster_accesSite.tab et roster_parcelles.tab -> <font color = "Blue">rga23_sites et rga23_parcelles</font>
- `rosterEngraisOrganiques.R` : création du fichier CSV correspondant au roster : roster_engrais_orga.tab -> <font color = "Blue">rga23_engraisOrga</font>

## Analyse

Ebauches des stats sur les différents profils.
### Non éligibles
- `nonEligibles.R`
### Eligibles
- `eligibles.R`
- `cultivateurs.R`
- `eleveurs.R`
- `coprahculteurs.R`
