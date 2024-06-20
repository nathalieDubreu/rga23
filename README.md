# Initialisation du projet

- Cloner ou télécharger le projet rga23
- Créer un dossier pour y stocker les différents fichiers CSV
- Créer un dossier nommé SortiesR dans ce dossier
- Créer un fichier nommé .Renviron à la racine du projet et renseigner la variable suivante vers le fichier de données :
    cheminAcces = C:/...

# Analyse des données du rga23

## Analyse
- `publicationDoubleTimbre.R` : contours de la première publi (avec Rmd associé)
- Programmes correspondant aux différentes demandes ponctuelles

## TCD - Exports de CSV pour faciliter la constitution des différents tableaux croisés dynamiques
- `ExportsTCD_IleCommune.R` : Décomposition par Archipel, Commune et Ile
### _Les programmes suivants nécessitent le lancement préalable de `ExportsTCD_IleCommune.R`_
- Divers programmes correspondant aux typologies existantes : `ExportsTCD_....R` 
### _Programme vierge à copier-coller,  renommer et compléter pour créér une nouvelle typologie_
- `ExportsTCD_TypologieACreer.R`

## Analyse supplémentaire : TAPE
- `tape.R`

## Champs
- `champRGA.R` : Champ du RGA23
- `champ2012.R` : Approximation des seuils de 2012
- `champCAPL.R` : Calcul des points CAPL
- `comparatifsAppartenances.R` : Comparatifs d'appartenance aux champs
- `champCAPL_tests.R` : Quelques tests unitaires

# Programmes des étapes préalables à l'analyse

## Mises en forme des données (à lancer dans l'ordre) => création des fichiers de données sous format CSV
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
- `rostersSurfaces.R` : 
    - récupération des 7 rosters de surfaces et regroupement en 1 seul fichier
    - imputation de valeurs pour SurfaceBio et SurfaceIrriguee (en fonction de AgriBio et Irrigation)
    - création du fichier csv correspondant -> **rga23_surfacesCultures**
- `rostersCommercialisation.R` :
    - imputation de la valeur 100 aux variables de part si une seule modalité est sélectionnée
    - récupération des valeurs présentes dans les rosters de commercialisation dans **rga23_prodAnimales** et **rga23_prodVegetales**
- `rosterCocoteraies.R` :
    - imputation de la valeur 100 à PartCoco si tout le revenu est conservé
    - création du fichier CSV correspondant au roster : roster_coco_loc.tab -> **rga23_cocoteraies**
- `rostersMainOeuvre.R` :
    - récupération des valeurs du roster MONonFamPerm.tab dans **rga23_mainOeuvre**
    - ajout des données récoltées par téléphone pour les X confrontés au bug dans la table rga23_moPermanenteFam
    - création de 2 fichiers CSV correspondant aux rosters RosterCoExploit.tab et RosterMOPermFam.tab (avec données X) -> **rga23_coexploitants** et **rga23_moPermanenteFam**
- `rostersParcellesEtSites.R` : création de 2 fichiers CSV correspondant aux rosters roster_accesSite.tab et roster_parcelles.tab -> **rga23_sites** et **rga23_parcelles**
- `ajoutIndicatrices.R` : ajout des indicatrices d'appartenance aux différents champs (inclus le lancement des programmes `champ....R`) dans **rga23_gestion**
### Au choix avant mise à disposition
#### A - Constitution de la base stat :
- `anonymisation.R` : suppression des variables d'identification des répondants et enquêteurs avant mise à disposition des fichiers pour la base de statistiques (debutCoprahculture, finCoprahculture debutLocalisation, gpsExploitation__Timestamp, AdressePhysiqueExploitation, finLocalisation AncienNom, AnciensPrenoms, AnciensTelephones, Nom, Prenoms, Telephone, Surnom, Email, AdressePhysique debutProdAnimales, finProdAnimales debutProdVegetales, finProdVegetales debutMainOeuvre, finMainOeuvre interview__id, id_enqueteur_ech, enqueteur, autre_enqueteur, sssys_irnd, has__errors, interview__status, assignment__id adresseSurfaceNonDelimitee, gps__Accuracy, gps__Altitude, gps__Latitude, gps__Longitude, gps__Timestamp)
- `modificationsFichiersBaseStat.R` : 
    - Récupération des variables de rga23_gestion dans le rga23_general (suppression du fichier **rga23_gestion**)
    - Nouvelle répartition des variables entre **rga23_general** et **rga23_mainOeuvre**
- `ajoutsIndicEtPointsCAPL.R` : Ajout du nombre de points CAPL calculés et d'une indicatrice de respect du seuil des 400 points ou de production de 2,7 tonnes ou plus de coprah
#### B - En vue de l'intégration dans le SIA
- `integrationSIA.R` : suppression des variables relatives à la situation conjugale de l'exploitant avant mise à disposition pour intégration dans le SIA

## Ajout juin 2024 : 
- `ajoutVariablesEtpEtRegroupementTables.R` : 
    - Sauvegarde des variables d'ETP dans une nouvelle table **rga23_etp**
    - Sauvegarde de la surface de productions végétales Hors pâturages et hors cocoteraie dans la table **rga23_prodVegetales** 
    - Regroupement des 9 fichiers plats pour faciliter le travail sous Excel **rga23_tousFichiersPlats**

## Collecte
### Mise à jour des fichiers Rmd en cours de collecte
- `comptages.R` : création d'un fichier md à destination de la Dag (comptages par statut de la collecte, par éligibilité, par île, ...)
- `stats.R` : génération d'un .md contenant qq stats descriptives sur données brutes
### Contrôles en cours de collecte
- Eligibilité (questionnaires complets or not)
	- `nonEligibles.R`
	- `eligibles.R`
- Divers profils
	- `cultivateurs.R`
	- `eleveurs.R`
	- `coprahculteurs.R`
