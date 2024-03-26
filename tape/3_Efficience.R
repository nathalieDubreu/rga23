# 3. EFFICIENCE

# UTILISATION D’INTRANTS EXTERIEURS
# Prenez en compte tous les intrants nécessaires à la production, y compris l’énergie, le carburant, les engrais, les semences, les jeunes animaux, la paille pour l’insémination artificielle, la main-d’œuvre, les substances phytosanitaires, etc.
# > 0 - Tous les intrants sont produits et achetés en dehors de l’agroécosystème.
# > 1 - La majorité des intrants sont achetés en dehors de l’agroécosystème.
# > 2 - Certains intrants sont produits au sein de l’agroécosystème ou échangés avec d’autres membres de la communauté.
# > 3 - La majorité des intrants sont produits au sein de l’agroécosystème ou échangés avec d’autres membres de la communauté..
# > 4 - Tous les intrants sont produits au sein de l’agroécosystème ou échangés avec d’autres membres de la communauté.

## RenouvAnimaux
# Le renouvellement de vos animaux est-il :
# Assuré sur la ferme...........................................1
# Produit localement à l'extérieur de la ferme (en Polynésie)...2
# Importé.......................................................3

# ComplAlimentation -> distinction entre autonomes ou pas du tout

# Plus de 90%.......................................1
# 75% à moins de 90%................................2
# 50% à moins de 75%................................3
# 25% à moins de 50%................................4
# Moins de 25%......................................5
# Aucune autonomie (tout est acheté)................6
# Sans objet, ce type d'aliment n'est pas utilisé...7

alimentsAchetesExclusivement <- (
  (rga23_prodAnimales$ComplAlimentation__2 == 1 |
    rga23_prodAnimales$ComplAlimentation__3 == 1 |
    rga23_prodAnimales$ComplAlimentation__4 == 1 |
    rga23_prodAnimales$ComplAlimentation__5 == 1) &
    rga23_prodAnimales$ComplAlimentation__1 == 0 &
    rga23_prodAnimales$ComplAlimentation__6 == 0 &
    rga23_prodAnimales$ComplAlimentation__7 == 0 &
    rga23_prodAnimales$ComplAlimentation__8 == 0 &
    rga23_prodAnimales$ComplAlimentation__9 == 0 &
    rga23_prodAnimales$ComplAlimentation__10 == 0)

alimentsPresentsExclusivement <- (
  (rga23_prodAnimales$ComplAlimentation__1 == 1 | rga23_prodAnimales$ComplAlimentation__7 == 1) &
    (rga23_prodAnimales$ComplAlimentation__2 == 0 &
      rga23_prodAnimales$ComplAlimentation__3 == 0 &
      rga23_prodAnimales$ComplAlimentation__4 == 0 &
      rga23_prodAnimales$ComplAlimentation__5 == 0 &
      rga23_prodAnimales$ComplAlimentation__6 == 0 &
      rga23_prodAnimales$ComplAlimentation__8 == 0 &
      rga23_prodAnimales$ComplAlimentation__9 == 0 &
      rga23_prodAnimales$ComplAlimentation__10 == 0
    )
)

autonomieAlimentaire <- function(niveau1, niveau2) {
  variables <- c(
    "AutAlimAnimauxBasseCour", "AutAlimBovinsFourrage", "AutAlimCaprinsFourrage",
    "AutAlimEquidesFourrages", "AutAlimOvinsFourrage", "AutAlimPorcins", "AutAlimPoules"
  )
  conditions <- lapply(variables, function(col) {
    paste0("(is.na(", paste("rga23_prodAnimales$", col, sep = ""), ") | ", paste("rga23_prodAnimales$", col, sep = ""), " == ", niveau1, " | ", paste("rga23_prodAnimales$", col, sep = ""), " == ", niveau2, ")")
  })
  condition <- paste0("(", paste(conditions, collapse = " & "), ")")
  return(condition)
}

autonomie_2_3_ <- autonomieAlimentaire(2, 3)
autonomie_5_6_ <- autonomieAlimentaire(5, 6)
autonomie_6_ <- autonomieAlimentaire(6,6)
autonomie_5_ <- autonomieAlimentaire(5,5)
autonomie_4_ <- autonomieAlimentaire(4,4)
autonomie_3_ <- autonomieAlimentaire(3,3)
autonomie_2_ <- autonomieAlimentaire(2,2)
autonomie_1_ <- autonomieAlimentaire(1,1)

rga23_prodAnimales_intrants <- rga23_prodAnimales |>
  mutate(
    alimentsAchetesExclusivement = case_when(
      alimentsAchetesExclusivement ~ 1,
      TRUE ~ as.numeric(NA)
    ),
    alimentsPresentsExclusivement = case_when(
      alimentsPresentsExclusivement ~ 1,
      TRUE ~ as.numeric(NA)
    ),
    niveauAutonomie = case_when(
      rowSums(across(
        c(
          "AutAlimAnimauxBasseCour", "AutAlimBovinsFourrage", "AutAlimCaprinsFourrage",
          "AutAlimEquidesFourrages", "AutAlimOvinsFourrage", "AutAlimPorcins", "AutAlimPoules"
        ),
        ~ coalesce(., 0)
      )) == 0 ~ as.numeric(NA),
      RaisonsRecensement__2 == 1 & eval(parse(text = autonomie_1_)) ~ 1,
      RaisonsRecensement__2 == 1 & eval(parse(text = autonomie_2_3_)) ~ 2.3,
      RaisonsRecensement__2 == 1 & eval(parse(text = autonomie_4_)) ~ 4,
      RaisonsRecensement__2 == 1 & eval(parse(text = autonomie_5_6_)) ~ 5.6,
      TRUE ~ 0
    )
  ) |>
  select(interview__key, niveauAutonomie, alimentsAchetesExclusivement, alimentsPresentsExclusivement)

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

# TypePhytosanit -> bio = 2 avec le cas échéant : AutoProdPhytoBio (oui/non/partie)

scoreIntrants <- left_join(rga23_tape,
  rga23_prodAnimales_intrants,
  by = "interview__key"
) |>
  left_join(
    rga23_exploitations,
    by = c("interview__key", "RaisonsRecensement__1", "RaisonsRecensement__2", "RaisonsRecensement__3")
  ) |>
  mutate(PartAutoproduction = case_when(
    ## Uniquement des semences
    UtilisationPlants == 2 & UtilisationGraines == 1 ~ PartSemencesAutoP,
    ## Uniquement des plants
    UtilisationPlants == 1 & UtilisationGraines == 2 ~ PartPlantsAutoP,
    ## Presence de plants et semences
    UtilisationPlants == 1 & UtilisationGraines == 1 ~ trunc((PartSemencesAutoP + PartPlantsAutoP) / 2),
    ## Ni l'un ni l'autre
    TRUE ~ as.numeric(NA)
  )) |>
  mutate(score = case_when(
    # > 0 - Tous les intrants sont produits et achetés en dehors de l’agroécosystème.
    (is.na(niveauAutonomie) | alimentsAchetesExclusivement == 1) &
      (is.na(ProvenancePlants__1) | ProvenancePlants__1 == 0) &
      (is.na(ProvenanceSemences__2) | ProvenanceSemences__2 == 0) &
      EnergiesRenouv == 2 ~ 0,
    # > 1 - La majorité des intrants sont achetés en dehors de l’agroécosystème.
    (is.na(niveauAutonomie) | niveauAutonomie == 5.6) &
      (is.na(PartAutoproduction) | PartAutoproduction == 1 | PartAutoproduction == 2) &
      (is.na(NivAutoEnergiesR) | NivAutoEnergiesR == 1) ~ 1,
    # > 2 - Certains intrants sont produits au sein de l’agroécosystème ou échangés avec d’autres membres de la communauté.
    (is.na(niveauAutonomie) | niveauAutonomie == 4) &
      (is.na(PartAutoproduction) | PartAutoproduction == 3) &
      (is.na(NivAutoEnergiesR) | NivAutoEnergiesR == 2) ~ 2,
    # > 3 - La majorité des intrants sont produits au sein de l’agroécosystème ou échangés avec d’autres membres de la communauté..
    (is.na(niveauAutonomie) | niveauAutonomie == 2.3) &
      (is.na(PartAutoproduction) | PartAutoproduction == 4) &
      (is.na(NivAutoEnergiesR) | NivAutoEnergiesR == 3) ~ 3,
    # > 3 - Tous les intrants sont produits au sein de l’agroécosystème ou échangés avec d’autres membres de la communauté MAIS pas ou peu d'énergie renouvelable produite
    (is.na(niveauAutonomie) | alimentsPresentsExclusivement == 1) &
      (is.na(ProvenancePlants__1) | PartPlantsAutoP == 5) &
      (is.na(ProvenanceSemences__2) | PartSemencesAutoP == 5 | ((ProvenanceSemences__2 == 1 | ProvenanceSemences__3 == 2) & ProvenanceSemences__1 == 0 & ProvenanceSemences__4 == 0)) &
      (is.na(NivAutoEnergiesR) | NivAutoEnergiesR <= 3) ~ 3,
    # > 4 - Tous les intrants sont produits au sein de l’agroécosystème ou échangés avec d’autres membres de la communauté.
    (is.na(niveauAutonomie) | alimentsPresentsExclusivement == 1) &
      (is.na(ProvenancePlants__1) | PartPlantsAutoP == 5) &
      (is.na(ProvenanceSemences__2) | PartSemencesAutoP == 5 | ((ProvenanceSemences__2 == 1 | ProvenanceSemences__3 == 2) & ProvenanceSemences__1 == 0 & ProvenanceSemences__4 == 0)) &
      NivAutoEnergiesR == 4 ~ 4,
    TRUE ~ 55
  ))

scoreIntrants |>
  group_by(score) |>
  count()

restent <- scoreIntrants |>
  filter(score == 55) |>
  group_by(NivAutoEnergiesR, niveauAutonomie, PartAutoproduction) |>
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

scoreEngrais <- rga23_exploitations |>
  mutate(score = case_when(
    ## Pas d'engrais ou uniquement "Engrais (ou amendements) de synthèse -> 0"
    is.na(UtilisationEngrais) | UtilisationEngrais == 2 | (TypeEngrais__1 == 1 & TypeEngrais__2 == 0 & TypeEngrais__3 == 0) ~ 0,
    ## Engrais de synthèse + engrais minéraux bio + engrais organiques avec engrais organiques présents utilisés à plus de 75% -> 3
    (TypeEngrais__1 == 1 & TypeEngrais__2 == 1 & TypeEngrais__3 == 1 & PropRecyclEngraisOrga == 4) ~ 3,
    ## Engrais de synthèse + engrais minéraux bio + engrais organiques -> 2
    (TypeEngrais__1 == 1 & TypeEngrais__2 == 1 & TypeEngrais__3 == 1) ~ 2,
    ## Engrais de synthèse + engrais minéraux bio -> 1
    (TypeEngrais__1 == 1 & TypeEngrais__2 == 1 & TypeEngrais__3 == 0) ~ 1,
    ## Engrais de synthèse + engrais organiques -> 1
    (TypeEngrais__1 == 1 & TypeEngrais__2 == 0 & TypeEngrais__3 == 1) ~ 1,
    ## Aucun engrais de synthèse -> 4
    (TypeEngrais__1 == 0 & (TypeEngrais__2 == 1 | TypeEngrais__3 == 1)) ~ 4,
    TRUE ~ 55
  ))

scoreEngrais |>
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

scorePesticides <- rga23_exploitations |> mutate(
  score = case_when(
    TypePhytosanit__1 == 0 & TypePhytosanit__2 == 1 ~ 3,
    UtilisationPhytosanit == 2 ~ 4,
    TypePhytosanit__1 == 1 & NbCultEspPhytoChim == 1 ~ 0,
    TypePhytosanit__1 == 1 & NbCultEspPhytoChim == 2 ~ 1,
    TypePhytosanit__1 == 1 & NbCultEspPhytoChim == 3 ~ 2,
    TRUE ~ 55
  )
)

scorePesticides |>
  group_by(score) |>
  count()


# PRODUCTIVITÉ ET BESOINS DU MÉNAGE
# Considérez tous les types d’actifs, y compris les animaux, les arbres vivaces, etc.
# > 0 - Les besoins du ménage ne sont pas satisfaits en nourriture ni en d’autres produits essentiels.
# > 1 - La production ne couvre que les besoins alimentaires du ménage. Pas de surplus pour générer des revenus.
# > 2 - La production couvre les besoins alimentaires du ménage et les excédents génèrent de l’argent pour acheter les produits essentiels mais ne permettent pas d’économiser.
# > 3 - La production couvre les besoins alimentaires du ménage et les excédents génèrent des liquidités pour acheter les produits essentiels et réaliser des économies sporadiques.
# > 4 - Tous les besoins du ménage sont satisfaits, à la fois en nourriture et en espèces, pour acheter tous les produits nécessaires et pour avoir des économies régulières.

scoreProductiviteBesoins <- rga23_tapeAvecVentes |>
  mutate(score = case_when(
    BesoinsSatisf == 2 ~ 0,
    (Economies == 3 & venteTypeProduits == 0) ~ 1,
    Economies == 3 ~ 2,
    Economies == 2 ~ 3,
    Economies == 1 ~ 4,
    TRUE ~ 55
  ))

scoreProductiviteBesoins |>
  group_by(score) |>
  count()
