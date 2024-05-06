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

score_1_MarchesLocaux <- left_join(
  left_join(rga23_tapeAvecVentes,
    rga23_prodAnimales,
    by = "interview__key"
  ),
  rga23_prodVegetales,
  by = "interview__key"
) |>
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
    TRUE ~ 55
  ))

score_1_MarchesLocaux |>
  group_by(score) |>
  count()

restent <- score_1_MarchesLocaux |>
  filter(score == 55) |>
  group_by(ComCulturesLocal, ComElevageLocal) |>
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
#
# SYSTÈME ALIMENTAIRE LOCAL
# > 0 - La communauté est totalement dépendante de l’extérieur pour l’achat de produits alimentaires et d’intrants agricoles et pour la commercialisation et la transformation des produits.
# > 1 - La majorité des approvisionnements alimentaires et des intrants agricoles sont achetés de l’extérieur et les produits sont transformés et commercialisés en dehors de la communauté locale. Très peu de biens et services sont échangés/vendus entre producteurs locaux.
# > 2 – L’approvisionnement alimentaire et les intrants sont achetés à l’extérieur de la communauté et/ou les produits sont transformés localement. Certains biens et services sont échangés/ vendus entre producteurs locaux.
# > 3 – Des parts égales de l’approvisionnement alimentaire et des intrants sont disponibles localement et achetés à l’extérieur de la communauté et les produits sont transformés localement. Les échanges/le commerce entre producteurs sont réguliers.
# > 4 - La communauté est presque entièrement autosuffisante pour la production agricole et alimentaire. Haut niveau d’échange/commerce de produits et services entre producteurs.
