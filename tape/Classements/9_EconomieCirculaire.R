# 9. ÉCONOMIE CIRCULAIRE & SOLIDAIRE
#
# 9.1. PRODUITS ET SERVICES COMMERCIALISÉS SUR LE MARCHÉ LOCAL
# > 0 - Aucun produit/service n’est commercialisé localement (ou pas assez de surplus produit), ou aucun marché local n’existe.
# > 1 - Les marchés locaux existent mais pratiquement aucun des produits/services n’est commercialisé localement.
# > 2 - Des marchés locaux existent. Certains produits/services sont commercialisés localement.
# > 3 - La plupart des produits/services sont commercialisés localement.
# > 4 - Tout est commercialisé localement.

# ComCulturesLocal et ComElevageLocal
# Aucun produit n’est commercialisé sur l’île car il n’y a pas de marché...1
# Des marchés possibles mais aucun produit commercialisé sur l’île.........2
# Certains produits sont commercialisés sur l’île..........................3
# La plupart des produits sont commercialisés sur l’île....................4
# Tout est commercialisé sur l’île.........................................5

## Récupération des points liés spécifiquement à l'élevage
source("champs/champCAPL.R")
rga23_prodAnimales_avecPointsElevages <- calculPointsCAPLElevage(rga23_prodAnimales)

score_1_MarchesLocaux <- left_join(
  left_join(rga23_tapeAvecVentes,
    rga23_prodAnimales_avecPointsElevages,
    by = "interview__key"
  ),
  rga23_prodVegetales,
  by = "interview__key"
) |>
  left_join(rga23_general |> select(interview__key, PointsCAPL),
    by = "interview__key"
  ) |>
  mutate(partElevage = nombrePointsElevages / PointsCAPL) |>
  mutate(score = case_when(
    (is.na(ComCulturesLocal) | ComCulturesLocal == 1 | venteProduitsVegetaux == 0) &
      (is.na(ComElevageLocal) | ComElevageLocal == 1 | venteProduitsAnimaux == 0) ~ 0,
    (is.na(ComCulturesLocal) | ComCulturesLocal == 2 | venteProduitsVegetaux == 0) &
      (is.na(ComElevageLocal) | ComElevageLocal == 2 | venteProduitsAnimaux == 0) ~ 1,
    (is.na(ComCulturesLocal) | ComCulturesLocal == 3 | venteProduitsVegetaux == 0) &
      (is.na(ComElevageLocal) | ComElevageLocal == 3 | venteProduitsAnimaux == 0) ~ 2,
    (is.na(ComCulturesLocal) | ComCulturesLocal == 4 | venteProduitsVegetaux == 0) &
      (is.na(ComElevageLocal) | ComElevageLocal == 4 | venteProduitsAnimaux == 0) ~ 3,
    (is.na(ComCulturesLocal) | ComCulturesLocal == 5 | venteProduitsVegetaux == 0) &
      (is.na(ComElevageLocal) | ComElevageLocal == 5 | venteProduitsAnimaux == 0) ~ 4,
    # Prise en compte de la part des points d'elevage dans le total
    (is.na(ComCulturesLocal) | ComCulturesLocal == 1 | partElevage > 2 / 3) &
      (is.na(ComElevageLocal) | ComElevageLocal == 1 | partElevage < 1 / 3) ~ 0,
    (is.na(ComCulturesLocal) | ComCulturesLocal == 2 | partElevage > 2 / 3) &
      (is.na(ComElevageLocal) | ComElevageLocal == 2 | partElevage < 1 / 3) ~ 1,
    (is.na(ComCulturesLocal) | ComCulturesLocal == 3 | partElevage > 2 / 3) &
      (is.na(ComElevageLocal) | ComElevageLocal == 3 | partElevage < 1 / 3) ~ 2,
    (is.na(ComCulturesLocal) | ComCulturesLocal == 4 | partElevage > 2 / 3) &
      (is.na(ComElevageLocal) | ComElevageLocal == 4 | partElevage < 1 / 3) ~ 3,
    (is.na(ComCulturesLocal) | ComCulturesLocal == 5 | partElevage > 2 / 3) &
      (is.na(ComElevageLocal) | ComElevageLocal == 5 | partElevage < 1 / 3) ~ 4,
    TRUE ~ 55
  ))

score_1_MarchesLocaux |>
  group_by(score) |>
  count()

# RÉSEAUX DE PRODUCTEURS, RELATIONS AVEC LES CONSOMMATEURS ET INTERMÉDIAIRES
# Avec perspective de genre
# > 0 - Il n’existe aucun réseau de producteurs pour commercialiser la production agricole.
# Aucune relation avec les consommateurs. Les intermédiaires gèrent l’ensemble du processus de commercialsation.
# > 1 - Les réseaux existent mais ne fonctionnent pas correctement.
# Peu de relations avec les consommateurs. Les intermédiaires gèrent la plupart du processus de commercialisation.
# > 2 - Les réseaux existent et sont opérationnels, mais n’incluent pas les femmes.
# Il existe une relation directe avec les consommateurs. Les intermédiaires gèrent une partie du processus de commericalisation.
# > 3 - Les réseaux existent, sont opérationnels, et comprennent les femmes.
# Il existe une relation directe avec les consommateurs. Les intermédiaires gèrent une partie du processus de commercialisation.
# > 4 - Des réseaux bien établis et opérationnels existent avec une participation égale des femmes.
# Relation solide et stable avec les consommateurs. Pas d’intermédiaires.

# Destination des types de produits
# Auto-consommation familiale.....................................1/1
# Alimentation des animaux .......................................2/2
# Dons (à la famille, des amis)...................................3/3
# Echange.........................................................4/4
# Vente directe au particulier ...................................5/5
# Vente par internet (Facebook ou autre site).....................6/6
# Vente à un commerçant, artisan ou revendeur.....................7/7
# Vente à un grossiste............................................8/8
# Vente à un transformateur ou préparateur (y compris abattoir)...9/9
# Vente à la coopérative ou au syndicat...........................10/10
# Vente à la restauration collective..............................11/11
# Vente aux restaurants (hors collectifs) / hôtels................12/12
# Sans objet (pas de production de ce type).......................13/13

score_2_ReseauxProducteurs <- full_join(rga23_prodVegetales,
  rga23_prodAnimales,
  by = "interview__key"
) |>
  left_join(rga23_mainOeuvre |> select(interview__key, SexeChefExpl)) |>
  mutate(
    score = case_when(
      # Ni 5 ni 10 ET part des intermédiaires (7,8,9) >= 75% pour au moins un type de vente
      eval(parse(text = partMaxParticuliersCooperatives_Vegetaux(0))) &
        eval(parse(text = partMaxParticuliersCooperatives_Animaux(0))) &
        (eval(parse(text = partMinIntermediaires_Vegetaux(75))) |
          eval(parse(text = partMinIntermediaires_Animaux(75))))
      ~ 0,
      # Si 5 ou 10 : <= 40 % de sa production ET part des intermédiaires (7,8,9) >= 50% pour au moins un type de vente
      eval(parse(text = partMaxParticuliersCooperatives_Vegetaux(40))) &
        eval(parse(text = partMaxParticuliersCooperatives_Animaux(40))) &
        (eval(parse(text = partMinIntermediaires_Vegetaux(50))) |
          eval(parse(text = partMinIntermediaires_Animaux(50))))
      ~ 1,
      # Si 5 ou 10 : au moins une fois >= 50 % de sa production ET part des intermédiaires (7,8,9) >= 20% pour au moins un type de vente ET chef d'exploitation est un homme
      (eval(parse(text = partMinParticuliersCooperatives_Vegetaux(50))) |
        eval(parse(text = partMinParticuliersCooperatives_Animaux(50)))) &
        (eval(parse(text = partMinIntermediaires_Vegetaux(20))) |
          eval(parse(text = partMinIntermediaires_Animaux(20)))) &
        SexeChefExpl == 2
      ~ 2,
      # Si 5 ou 10 : au moins une fois >= 50 % de sa production ET part des intermédiaires (7,8,9) >= 20% pour au moins un type de vente ET chef d'exploitation est un femme
      (eval(parse(text = partMinParticuliersCooperatives_Vegetaux(50))) |
        eval(parse(text = partMinParticuliersCooperatives_Animaux(50)))) &
        (eval(parse(text = partMinIntermediaires_Vegetaux(20))) |
          eval(parse(text = partMinIntermediaires_Animaux(20)))) &
        SexeChefExpl == 1
      ~ 3,
      # Pas de 7 ni 8 ni 9
      eval(parse(text = aucunIntermediaire_Vegetaux())) & eval(parse(text = aucunIntermediaire_Animaux())) ~ 4,
      TRUE ~ 55
    )
  )

score_2_ReseauxProducteurs |>
  group_by(score) |>
  count()

# SYSTÈME ALIMENTAIRE LOCAL
# > 0 - La communauté est totalement dépendante de l’extérieur pour l’achat de produits alimentaires et d’intrants agricoles et pour la commercialisation et la transformation des produits.
# > 1 - La majorité des approvisionnements alimentaires et des intrants agricoles sont achetés de l’extérieur et les produits sont transformés et commercialisés en dehors de la communauté locale. Très peu de biens et services sont échangés/vendus entre producteurs locaux.
# > 2 – L’approvisionnement alimentaire et les intrants sont achetés à l’extérieur de la communauté et/ou les produits sont transformés localement. Certains biens et services sont échangés/ vendus entre producteurs locaux.
# > 3 – Des parts égales de l’approvisionnement alimentaire et des intrants sont disponibles localement et achetés à l’extérieur de la communauté et les produits sont transformés localement. Les échanges/le commerce entre producteurs sont réguliers.
# > 4 - La communauté est presque entièrement autosuffisante pour la production agricole et alimentaire. Haut niveau d’échange/commerce de produits et services entre producteurs.

# - Provenance semences :
# Commercialisées localement..................1
# Auto-produites..............................2
# Fournies par d'autres agriculteurs (dons)...3
# Importées par vous-même.....................4

#  - Part d'autoproduction des semences
# 0 à 10% du volume utilisé.......1
# 10 à 25% du volume utilisé......2
# 25 à 50% du volume utilisé......3
# 50 à 75% du volume utilisé......4
# Plus de 75% du volume utilisé...5

# - RenouvAnimaux :
# Assuré sur la ferme...........................................1
# Produit localement à l'extérieur de la ferme (en Polynésie)...2
# Importé.......................................................3

# - TransformationPA : Réalisez-vous la transformation de produits agricoles (y compris si la matière première est achetée ailleurs) ?
# Transformation d’oléagineux (huiles de consommation ou pour combustible…).......................1/1
# Production d’huiles essentielles et hydrolats...................................................2/2
# Epluchage, découpe, conditionnement, 4ème gamme, ...............................................3/3
# Transformation de légumes (soupe, conserves…)...................................................4/4
# Transformation de racines ou tubercules (farine de manioc, kwak...).............................5/5
# Production issue de canne à sucre (jus de canne, rhum...).......................................6/6
# Transformation de vanille.......................................................................7/7
# Transformation de produits du cocotier (coprah, niau, lait de coco, …)..........................8/8
# Transformation de fruits (confitures, sirops, liqueurs, jus de fruits…).........................9/9
# Transformation de lait (beurre, yaourts, fromages, crème...)....................................10/10
# Abattage à la ferme.............................................................................11/11
# Transformation de viandes (pâtés, salaisons, conserves…)........................................12/12
# Découpe de viandes, caissettes….................................................................13/13
# Production de produits à base de miel...........................................................14/14
# Transformation d’autres produits agricoles (hors aliments pour les animaux de l’exploitation)...15/15
# Préparation de plats cuisinés...................................................................16/16
# Aucune transformation réalisée sur l'exploitation...............................................17/17

transformationsPossibles <- paste0("TransformationPA__", 1:16)

score_3_SystAlimLocal <- rga23_exploitations |>
  left_join(
    rga23_mainOeuvre |>
      mutate(nbTransformations = rowSums(across(
        all_of(transformationsPossibles),
        ~ coalesce(., 0)
      ))) |> select(interview__key, nbTransformations),
    by = "interview__key"
  ) |>
  left_join(
    rga23_prodAnimales |>
      mutate(presencePoulesPondeuses = case_when(
        TypeVolailles__1 == 1 ~ 1,
        TypeVolailles__3 == 1 ~ 1,
        TypeVolailles__4 == 1 ~ 1,
        TRUE ~ 0
      )) |> select(interview__key, RenouvAnimaux__1, RenouvAnimaux__2, RenouvAnimaux__3, presencePoulesPondeuses),
    by = "interview__key"
  ) |>
  mutate(
    score = case_when(

      # Aucune transformation réalisée sur l'exploitation + 
      # Provenance semences uniquement Importées et/ou commercialisés localement + 
      # Renouvellement animaux importé
      nbTransformations == 0 &
        (is.na(UtilisationGraines) | UtilisationGraines == 2 |
          (ProvenanceSemences__1 == 1 | ProvenanceSemences__4 == 1) & ProvenanceSemences__2 == 0 & ProvenanceSemences__3 == 0) &
        (RaisonsRecensement__2 == 0 | (RenouvAnimaux__1 == 0 & RenouvAnimaux__2 == 0 & RenouvAnimaux__3 == 1)) ~ 0,
      
      # Aucune transformation réalisée sur l'exploitation 
      nbTransformations == 0 ~ 1,
      
      # Exactement une transformation + 
      # provenances semences en partie auto-produites (entre 10 et 50%) ou fournies par d'autres agriculteurs +
      # renouv animaux en partie assuré par la ferme ou produit localement
      nbTransformations == 1 &
        (is.na(UtilisationGraines) | UtilisationGraines == 2 | PartSemencesAutoP == 2 | PartSemencesAutoP == 3 | ProvenanceSemences__3 == 1) &
        (RaisonsRecensement__2 == 0 | RenouvAnimaux__1 == 1 | RenouvAnimaux__2 == 1) ~ 2,
      
      # Au moins une transformation + 
      # part d'autoproduction des semences entre 50 et 75% + 
      # si élevage, pas de poules pondeuses et renouvellement uniquement assuré sur la ferme
      nbTransformations >= 1 &
        (is.na(UtilisationGraines) | UtilisationGraines == 2 | PartSemencesAutoP == 4) &
        (RaisonsRecensement__2 == 0 | (presencePoulesPondeuses == 0 & RenouvAnimaux__2 == 0 & RenouvAnimaux__3 == 0)) ~ 3,
      
      # Au moins une transformation + 
      # part d'autoproduction des semences > 75% + 
      # si élevage, pas de poules pondeuses et renouvellement uniquement assuré sur la ferme
      nbTransformations >= 1 &
        (is.na(UtilisationGraines) | UtilisationGraines == 2 | PartSemencesAutoP == 5) &
        (RaisonsRecensement__2 == 0 | (presencePoulesPondeuses == 0 & RenouvAnimaux__2 == 0 & RenouvAnimaux__3 == 0)) ~ 4,
      
      TRUE ~ 55
    )
  )

score_3_SystAlimLocal |>
  group_by(score) |>
  count()

restent <- score_3_SystAlimLocal |>
  filter(score == 55) |>
  group_by(score, ProvenanceSemences__1, ProvenanceSemences__2, UtilisationGraines, nbTransformations, RaisonsRecensement__2, presencePoulesPondeuses,
           RenouvAnimaux__1,
           RenouvAnimaux__2,
           RenouvAnimaux__3) |>
  count()
