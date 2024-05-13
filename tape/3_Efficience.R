# 3. EFFICIENCE

# UTILISATION D’INTRANTS EXTERIEURS
# Prenez en compte tous les intrants nécessaires à la production, y compris l’énergie, le carburant, les engrais, les semences, les jeunes animaux, la paille pour l’insémination artificielle, la main-d’œuvre, les substances phytosanitaires, etc.
# > 0 - Tous les intrants sont produits et achetés en dehors de l’agroécosystème.
# > 1 - La majorité des intrants sont achetés en dehors de l’agroécosystème.
# > 2 - Certains intrants sont produits au sein de l’agroécosystème ou échangés avec d’autres membres de la communauté.
# > 3 - La majorité des intrants sont produits au sein de l’agroécosystème ou échangés avec d’autres membres de la communauté..
# > 4 - Tous les intrants sont produits au sein de l’agroécosystème ou échangés avec d’autres membres de la communauté.

## ProvenancePlants -> autoproduits = 1 (avec PartPlantsAutoP)
# 0 à 10% du volume utilisé.......1
# 10 à 25% du volume utilisé......2
# 25 à 50% du volume utilisé......3
# 50 à 75% du volume utilisé......4
# Plus de 75% du volume utilisé...5

## ProvenanceSemences -> autoproduits = 2 (avec PartSemencesAutoP)
## ProvenanceSemences -> Fournies par d'autres agriculteurs (dons) = 3

## EnergiesRenouv avec le niveau d'autonomie : NivAutoEnergiesR
# 25%....1
# 50%....2
# 75%....3
# 100%...4

score_1_Intrants <- left_join(rga23_tape,
  rga23_prodAnimales_alimentation,
  by = c("interview__key", "RaisonsRecensement__1", "RaisonsRecensement__2", "RaisonsRecensement__3")
) |>
  left_join(
    rga23_exploitations,
    by = c("interview__key", "RaisonsRecensement__1", "RaisonsRecensement__2", "RaisonsRecensement__3")
  ) |>
  mutate(
    MaterielNecessitantEnergie = case_when(
      MaterielTransport__1 == 1 ~ 1,
      MaterielTransport__2 == 1 ~ 1,
      MaterielTransport__3 == 1 ~ 1,
      MaterielTransport__4 == 1 ~ 1,
      MaterielTransport__5 == 1 ~ 1,
      MaterielTransport__6 == 1 ~ 1,
      MaterielTransport__7 == 1 ~ 1,
      MaterielTransport__8 == 1 ~ 1,
      MaterielTransport__9 == 1 ~ 1,
      MaterielTransport__10 == 1 ~ 1,
      MaterielTransport__11 == 1 ~ 1,
      MaterielTraitRecolte__6 == 1 ~ 1,
      TRUE ~ 0
    ),
    PartSemencesAutoP_Ech = case_when(
      ## Uniquement de l'auto-production ou de l'échange -> part réévaluée à 5
      (ProvenanceSemences__2 == 1 | ProvenanceSemences__3 == 2) & ProvenanceSemences__1 == 0 & ProvenanceSemences__4 == 0 ~ 5,
      TRUE ~ PartSemencesAutoP
    ),
    PartAutoproduction = case_when(
      ## Uniquement des semences échangées
      UtilisationPlants == 2 & UtilisationGraines == 1 ~ PartSemencesAutoP_Ech,
      ## Uniquement des plants
      UtilisationPlants == 1 & UtilisationGraines == 2 ~ PartPlantsAutoP,
      ## Presence de plants et semences
      UtilisationPlants == 1 & UtilisationGraines == 1 ~ trunc((PartSemencesAutoP_Ech + PartPlantsAutoP) / 2),
      ## Ni l'un ni l'autre
      TRUE ~ as.numeric(NA)
    ),
    niveauAutonomieInv = case_when(
      # Au moins 75 %
      niveauAutonomie <= 2 ~ 5,
      # 50 à 75% (voire 90%)
      niveauAutonomie <= 3 ~ 4,
      # 25 à 50% (voire 75%)
      niveauAutonomie <= 4 ~ 3,
      # 1 à 25% (voire 50%)
      niveauAutonomie <= 5 ~ 2,
      # 0 % (voire une partie jusqu'à 25%)
      niveauAutonomie <= 6 ~ 1,
      TRUE ~ as.numeric(NA)
    ),
    notePonderee = case_when(
      # Les 3 sont présents
      ## Part d'auto-production des plants et semences = 60% de la note
      ## Autonomie alimentaire des animaux calculée = 25% de la note
      ## Niveau d'énergie renouvelable (+ 1 pour replacer l'échelle des notes jusque 5) = 15% de la note
      EnergiesRenouv == 1 & !is.na(niveauAutonomieInv) & !is.na(PartAutoproduction) ~ trunc(0.15 * (NivAutoEnergiesR + 1) + 0.25 * niveauAutonomieInv + 0.60 * PartAutoproduction),
      # Pas de production d'énergie renouvelable :
      ## Autonomie alimentaire des animaux calculée = 35% de la note
      ## Part d'auto-production des plants et semences = 65% de la note
      ## =====> Déclassement d'une catégorie à cause de l'absence d'énergie ???
      EnergiesRenouv == 2 & !is.na(niveauAutonomieInv) & !is.na(PartAutoproduction) ~ trunc(0.35 * niveauAutonomieInv + 0.65 * PartAutoproduction) - MaterielNecessitantEnergie,
      # Pas d'animaux dont on contrôle l'autonomie alimentaire
      ## Part d'auto-production des plants et semences = 65% de la note
      ## Niveau d'énergie renouvelable (+ 1 pour replacer l'échelle des notes jusque 5) = 35% de la note
      EnergiesRenouv == 1 & is.na(niveauAutonomie) & !is.na(PartAutoproduction) ~ trunc(0.35 * (NivAutoEnergiesR + 1) + 0.65 * PartAutoproduction),
      # Pas de semences ni plants
      ## Autonomie alimentaire des animaux = 65% de la note
      ## Niveau d'énergie renouvelable (+ 1 pour replacer l'échelle des notes jusque 5) = 35% de la note
      is.na(PartAutoproduction) & EnergiesRenouv == 1 & !is.na(niveauAutonomieInv) ~ trunc(0.35 * (NivAutoEnergiesR + 1) + 0.65 * niveauAutonomieInv),
      # Pas de semences ni plants + pas d'énergie renouvelable produite
      ## Autonomie alimentaire des animaux calculée
      ## =====> Déclassement d'une catégorie à cause de l'absence d'énergie ???
      is.na(PartAutoproduction) & EnergiesRenouv == 2 & !is.na(niveauAutonomieInv) ~ niveauAutonomieInv - MaterielNecessitantEnergie,
      # Pas d'animaux dont on contrôle l'autonomie alimentaire + pas d'énergie renouvelable produite
      ## Part d'auto-production des plants et semences
      ## =====> Déclassement d'une catégorie à cause de l'absence d'énergie ???
      !is.na(PartAutoproduction) & EnergiesRenouv == 2 & is.na(niveauAutonomie) ~ PartAutoproduction - MaterielNecessitantEnergie,
      TRUE ~ 55
    )
  ) |>
  mutate(score = case_when(
    # > 0 - Tous les intrants sont produits et achetés en dehors de l’agroécosystème.
    (is.na(niveauAutonomie) | alimentsAchetesExclusivement == 1) &
      (is.na(ProvenancePlants__1) | ProvenancePlants__1 == 0) &
      (is.na(ProvenanceSemences__2) | ProvenanceSemences__2 == 0) &
      EnergiesRenouv == 2 ~ 0,
    # > 1 - La majorité des intrants sont achetés en dehors de l’agroécosystème.
    (is.na(niveauAutonomie) | niveauAutonomie == 5 | niveauAutonomie == 6 | niveauAutonomie == 5.5) &
      (is.na(PartAutoproduction) | PartAutoproduction == 1 | PartAutoproduction == 2) &
      (is.na(NivAutoEnergiesR) | NivAutoEnergiesR == 1) ~ 1,
    # > 2 - Certains intrants sont produits au sein de l’agroécosystème ou échangés avec d’autres membres de la communauté.
    (is.na(niveauAutonomie) | niveauAutonomie == 4) &
      (is.na(PartAutoproduction) | PartAutoproduction == 3) &
      (NivAutoEnergiesR == 1 | NivAutoEnergiesR == 2) ~ 2,
    # > 3 - La majorité des intrants sont produits au sein de l’agroécosystème ou échangés avec d’autres membres de la communauté..
    (is.na(niveauAutonomie) | niveauAutonomie == 2 | niveauAutonomie == 3 | niveauAutonomie == 2.5) &
      (is.na(PartAutoproduction) | PartAutoproduction == 4) &
      (NivAutoEnergiesR == 2 | NivAutoEnergiesR == 3) ~ 3,
    # > 4 - Tous les intrants sont produits au sein de l’agroécosystème ou échangés avec d’autres membres de la communauté.
    (is.na(niveauAutonomie) | alimentsPresentsExclusivement == 1) &
      (is.na(PartAutoproduction) | PartAutoproduction == 5) &
      NivAutoEnergiesR == 4 ~ 4,
    # Pondération pour le reste des exploitations
    notePonderee <= 1 ~ 0,
    notePonderee == 2 ~ 1,
    notePonderee == 3 ~ 2,
    notePonderee == 4 ~ 3,
    notePonderee == 5 ~ 4,
    TRUE ~ 55
  ))

score_1_Intrants |>
  group_by(score) |>
  count()

# GESTION DE LA FERTILITÉ DU SOL
# > 0 - Les engrais synthétiques sont utilisés régulièrement sur toutes les cultures et / ou prairies (ou aucun engrais n’est utilisé par manque d’accès, mais aucun autre système de gestion n’est utilisé).
# > 1 - Les engrais synthétiques sont utilisés régulièrement sur la plupart des cultures et certaines pratiques biologiques (par exemple le fumier ou le compost) sont appliquées à certaines cultures et / ou prairies.
# > 2 - Les engrais synthétiques ne sont utilisés que sur quelques cultures spécifiques. Des pratiques biologiques sont appliquées aux autres cultures et / ou prairies.
# > 3 - Les engrais synthétiques ne sont utilisés qu’exceptionnellement. Une variété de pratiques biologiques sont la norme.
# > 4 - Aucun engrais synthétique n’est utilisé, la fertilité du sol est gérée uniquement à travers une variété de pratiques biologiques.

# Engrais (ou amendements) de synthèse............1
# Engrais (ou amendements) minéraux biologiques...3
# Engrais (ou amendements) organiques.............2

# Ajout 08/04/2024 : Discrimination entre les catégories 0 et 1 et entre les catégories 3 et 4 si l'une au moins des pratiques est utilisée
# Utilisation de plantes de services.............1
# Intercultures..................................2
# Amendements calciques..........................6
# Utilisation de micro-organismes du sol.........7
# OU jardins océaniens
# Ou agroforesterie

# + Ajout cultures concernées par le chimique

score_2_Engrais <- left_join(rga23_exploitations,
  rga23_prodVegetales |> select(interview__key, SurfaceJardins, SurfaceAgroF),
  by = "interview__key"
) |>
  left_join(rga23_tape |> select(interview__key, PratiquesCulturales__1, PratiquesCulturales__2, PratiquesCulturales__6, PratiquesCulturales__7),
    by = "interview__key"
  ) |>
  mutate(
    nombrePratiques =
      replace_na(SurfaceJardins, 0) +
        replace_na(SurfaceAgroF, 0) +
        replace_na(PratiquesCulturales__1, 0) +
        replace_na(PratiquesCulturales__2, 0) +
        replace_na(PratiquesCulturales__6, 0) +
        replace_na(PratiquesCulturales__7, 0),
    score = case_when(
      ## 0 - Pas d'engrais organiques ni minéraux biologiques ET aucune des pratiques considérées
      (is.na(UtilisationEngrais) | UtilisationEngrais == 2 | (TypeEngrais__1 == 1 & TypeEngrais__2 == 0 & TypeEngrais__3 == 0)) & nombrePratiques == 0 ~ 0,
      ## 1 - Pas d'engrais organiques ni minéraux biologiques ET au moins une des pratiques considérées
      (is.na(UtilisationEngrais) | UtilisationEngrais == 2 | (TypeEngrais__1 == 1 & TypeEngrais__2 == 0 & TypeEngrais__3 == 0)) & nombrePratiques >= 1 ~ 1,
      ## 3 - Engrais de synthèse + engrais minéraux bio et/ou engrais organiques et au moins deux pratiques considérées ou produits phyto sur 0 ou une seule espèce/culture
      TypeEngrais__1 == 1 & (TypeEngrais__2 == 1 | TypeEngrais__3 == 1) & (nombrePratiques >= 2 | NbCultEspPhytoChim == 3 | is.na(NbCultEspPhytoChim)) ~ 3,
      ## 2 - Engrais de synthèse  + engrais minéraux bio et/ou engrais organiques + produits phyto sur une partie des cultures et espèces
      TypeEngrais__1 == 1 & NbCultEspPhytoChim == 2 & (TypeEngrais__2 == 1 | TypeEngrais__3 == 1) ~ 2,
      ## 1 - Engrais de synthèse + engrais minéraux bio et/ou engrais organiques + produits phyto sur toutes les cultures et espèces
      TypeEngrais__1 == 1 & NbCultEspPhytoChim == 1 & (TypeEngrais__2 == 1 | TypeEngrais__3 == 1) ~ 1,
      ## 3 - Aucun engrais de synthèse mais minéraux et/ou organiques et pas de pratiques
      (TypeEngrais__1 == 0 & (TypeEngrais__2 == 1 | TypeEngrais__3 == 1)) & nombrePratiques == 0 ~ 3,
      ## 4 - Aucun engrais de synthèse mais minéraux et/ou organiques et au moins une pratique
      (TypeEngrais__1 == 0 & (TypeEngrais__2 == 1 | TypeEngrais__3 == 1)) & nombrePratiques >= 1 ~ 4,
      TRUE ~ 55
    )
  )

score_2_Engrais |>
  group_by(score) |>
  count()

# GESTION DES PESTES ET DES MALADIES
# > 0 - Les pesticides chimiques et les médicaments sont utilisés régulièrement pour la lutte contre les ravageurs et les maladies. Aucune autre gestion n’est utilisée.
# > 1 - Les pesticides et médicaments chimiques sont utilisés pour une culture/un animal spécifique uniquement. Certaines substances biologiques et pratiques organiques sont appliquées sporadiquement.
# > 2 – Les ravageurs et les maladies sont gérés par des pratiques biologiques, mais les pesticides chimiques sont utilisés seulement dans des cas spécifiques et très limités.
# > 3 – Aucun pesticide ni médicament chimique n’est utilisé. Les substances biologiques sont la norme.
# > 4 - Aucun pesticide ni médicament chimique n’est utilisé. Les ravageurs et les maladies sont gérés par une variété de substances biologiques et de mesures de prévention.
#

# UtilisationPhytosanit

# TypePhytosanit
# Synthétique (chimique)...1
# Biologique...............2

# NbCultEspPhytoChim
# Toutes vos cultures/espèces..........1
# Une partie de vos cultures/espèces...2
# Une seule culture/espèce.............3

score_3_Pesticides <- rga23_exploitations |> mutate(
  score = case_when(
    TypePhytosanit__1 == 1 & NbCultEspPhytoChim == 1 ~ 0,
    UtilisationGlyphosate == 1 ~ 1,
    TypePhytosanit__1 == 1 & NbCultEspPhytoChim == 2 ~ 1,
    TypePhytosanit__1 == 1 & NbCultEspPhytoChim == 3 ~ 2,
    TypePhytosanit__1 == 0 & TypePhytosanit__2 == 1 ~ 3,
    UtilisationPhytosanit == 2 ~ 4,
    TRUE ~ 55
  )
)

score_3_Pesticides |>
  group_by(score) |>
  count()


# PRODUCTIVITÉ ET BESOINS DU MÉNAGE
# Considérez tous les types d’actifs, y compris les animaux, les arbres vivaces, etc.
# > 0 - Les besoins du ménage ne sont pas satisfaits en nourriture ni en d’autres produits essentiels.
# > 1 - La production ne couvre que les besoins alimentaires du ménage. Pas de surplus pour générer des revenus.
# > 2 - La production couvre les besoins alimentaires du ménage et les excédents génèrent de l’argent pour acheter les produits essentiels mais ne permettent pas d’économiser.
# > 3 - La production couvre les besoins alimentaires du ménage et les excédents génèrent des liquidités pour acheter les produits essentiels et réaliser des économies sporadiques.
# > 4 - Tous les besoins du ménage sont satisfaits, à la fois en nourriture et en espèces, pour acheter tous les produits nécessaires et pour avoir des économies régulières.

score_4_ProductiviteBesoins <- rga23_tapeAvecVentes |>
  mutate(score = case_when(
    BesoinsSatisf == 2 ~ 0,
    (Economies == 3 & venteTypeProduits == 0) ~ 1,
    Economies == 3 ~ 2,
    Economies == 2 ~ 3,
    Economies == 1 ~ 4,
    TRUE ~ 55
  ))

score_4_ProductiviteBesoins |>
  group_by(score) |>
  count()
