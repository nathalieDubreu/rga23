# rga23

## Mises en forme des données (à lancer dans l'ordre) => création des 16 fichiers de données
- `interviewKeysAExclure.R` : liste des interview__key à exclure (affectations en doublon dans SuSo - pb techniques)
- `rosterEngraisOrganiques.R` : création du fichier CSV correspondant au roster : roster_engrais_orga.tab -> **rga23_engraisOrga**
- `imputationVariables.R` :
    - impute les valeurs pour certaines variables *(AbeillesBio, PartPlantsAutoP, PartRevenusAgriExpl, PartSemencesAutoP, ...)* en fonction des valeurs d'autres variables
    - traitement des noms, prénoms et téléphones corrigés + passage des NSP (valeur par défaut = 1) en somme des surfaces déclarées dans le détail pour la SAU et en NA pour la surface de végétation naturelle
- `correctionsAlimentationAnimaux.R` : Certaines questions, notamment issues de TAPE, n’ont pas été comprises par tous les exploitants et ont nécessité quelques redressements a posteriori : 
        - Ajout de la modalité "Fourrages produit localement" dans le listing des aliments pour les animaux pour les éleveurs qui ont des caprins et/ou des bovins élevés au moins partiellement en plein air ;
        - Ajout de la modalité "Aliments complets commercialisés importés" pour les éleveurs qui ont plus de 100 poules pondeuses de catégorie 1, 2 ou 3.
Ensuite, de façon automatique, la réponse "Aucune autonomie" a été imputée dès lors que tous les aliments sont achetés à l'extérieur de l'exploitation et a contrario la case "Plus de 90% d'autonomie" si au contraire, les seuls aliments consommés par les animaux présents sont fourrages et écarts de tris.
- `correctionsValorisationEngrais.R` : La variable relative à la valorisation des engrais organiques  a été redressée à moins de 25% dès lors que les engrais ne sont ni donnés, ni vendus ni épandus sur les exploitations.
- `ajoutDonneesX.R` : Récupération des données relatives aux X obtenues par téléphone (hors variables spécifiques au roster MO permanente familiale - cf. rostersMainOeuvre.R)
- `ajoutVariableBio.R` : Ajout d'une variable reprenant les valeurs de AgriBio sauf pour les exploitants identifiés par la DAG
- `modificationsIdDoublonsEtX.R` :
    - Passage des identifiants X en P s'ils ne font plus de coprah
    - Passage des identifiants X en C s'ils ne font que du coprah
    - Passage des identifiants C ou P en X en cas de doublons
- `decoupageRga23.R` : découpage du fichier rga23 global en 9 sous fichiers (**rga23_coprahculteurs**, **rga23_exploitations**, **rga23_general**, rga23_mainOeuvre, rga23_prodAnimales, rga23_prodVegetales, rga23_gestion, **rga23_peche** et **rga23_tape**)
- `ajoutIndicatrices.R` : ajout des indicatrices d'appartenance aux différents champs (inclus le lancement des programmes `champ....R`) dans **rga23_gestion**
- `rostersCommercialisation.R` :
    - imputation de la valeur 100 aux variables de part si une seule modalité est sélectionnée
    - récupération des valeurs présentes dans les rosters de commercialisation dans **rga23_prodAnimales** et **rga23_prodVegetales**
- `rostersSurfaces.R` : 
    - récupération des 7 rosters de surfaces et regroupement en 1 seul fichier
    - imputation de valeurs pour SurfaceBio et SurfaceIrriguee (en fonction de AgriBio et Irrigation)
    - création du fichier csv correspondant -> **rga23_surfacesCultures**
- `rosterCocoteraies.R` :
    - imputation de la valeur 100 à PartCoco si tout le revenu est conservé
    - création du fichier CSV correspondant au roster : roster_coco_loc.tab -> **rga23_cocoteraies**
- `rostersMainOeuvre.R` :
    - récupération des valeurs du roster MONonFamPerm.tab dans **rga23_mainOeuvre**
    - ajout des données récoltées par téléphone pour les X confrontés au bug dans la table rga23_moPermanenteFam
    - création de 2 fichiers CSV correspondant aux rosters RosterCoExploit.tab et RosterMOPermFam.tab (avec données X) -> **rga23_coexploitants** et **rga23_moPermanenteFam**
- `rostersParcellesEtSites.R` : création de 2 fichiers CSV correspondant aux rosters roster_accesSite.tab et roster_parcelles.tab -> **rga23_sites** et **rga23_parcelles**
### Au choix avant mise à disposition
- `anonymisation.R` : suppression des variables d'identification des répondants et enquêteurs avant mise à disposition des fichiers pour la base de statistiques (debutCoprahculture, finCoprahculture debutLocalisation, gpsExploitation__Timestamp, AdressePhysiqueExploitation, finLocalisation AncienNom, AnciensPrenoms, AnciensTelephones, Nom, Prenoms, Telephone, Surnom, Email, AdressePhysique debutProdAnimales, finProdAnimales debutProdVegetales, finProdVegetales debutMainOeuvre, finMainOeuvre interview__id, id_enqueteur_ech, enqueteur, autre_enqueteur, sssys_irnd, has__errors, interview__status, assignment__id adresseSurfaceNonDelimitee, gps__Accuracy, gps__Altitude, gps__Latitude, gps__Longitude, gps__Timestamp)
- `integrationSIA.R` : suppression des variables relatives à la situation conjugale de l'exploitant avant mise à disposition pour intégration dans le SIA

## Champs - WIP
- `champRGA.R` : Champ du RGA23
    - TODO : traiter les jardins océaniens avec une seule culture notamment
- `champ2012.R` : Approximation des seuils de 2012
- `indicatricesAppartenances.R` : Ajout des indicatrices d'appartenance aux champs dans le fichier rga23.csv
- `champCAPL.R` : Calcul des points CAPL
  
## Analyse
- `stats.R` : génération d'un .md contenant qq stats descriptives sur données brutes
- `publicationDoubleTimbre.R` : contours de la première publi(avec Rmd associé)

## Collecte
### Mise à jour du fichier Rmd
- `comptages.R` : création d'un fichier md à destination de la Dag (comptages par statut de la collecte, par éligibilité, par île, ...)
### Contrôles en cours de collecte
- Eligibilité (questionnaires complets or not)
	- `nonEligibles.R`
	- `eligibles.R`
- Divers profils
	- `cultivateurs.R`
	- `eleveurs.R`
	- `coprahculteurs.R`
