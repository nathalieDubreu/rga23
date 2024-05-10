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

score_3_SystAlimLocal <- rga23_tape |>
  mutate(
    score = case_when(
      TRUE ~ 55
    )
  )

score_3_SystAlimLocal |>
  group_by(score) |>
  count()
