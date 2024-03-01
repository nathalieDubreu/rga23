## Alimentation des animaux

## Aliments
# Fourrage produit localement..................1/1
# Céréales.....................................2/2
# Aliments complets commercialisés importés....3/3
# Aliments complets commercialisés locaux......4/4
# Tourteau de coprah...........................5/5
# Eaux grasses.................................6/6
# Ecarts de tri / déchets de culture...........7/7
# Déchets industriels..........................8/8
# Compléments alimentaires (pierre à lécher)...9/9
# Autre........................................10/10

## Autonomie
# Plus de 90%.......................................1
# 75% à moins de 90%................................2
# 50% à moins de 75%................................3
# 25% à moins de 50%................................4
# Moins de 25%......................................5
# Aucune autonomie (tout est acheté)................6
# Sans objet, ce type d'aliment n'est pas utilisé...7

## 1 - Bovins et/ou caprins au moins en partie en plein air
# => "Fourrage produit localement" (1) à cocher dans l'alimentation

rga23 |>
  filter((nbTotalBovins > 0 & BovinsPleinAir != 2) |
    (nbTotalCaprins > 0 & CaprinsPleinAir != 2)) |>
  filter(ComplAlimentation__1 == 0) |>
  count()

rga23 <- rga23 |> mutate(ComplAlimentation__1 = case_when(
  (nbTotalBovins > 0 & BovinsPleinAir != 2) |
    (nbTotalCaprins > 0 & CaprinsPleinAir != 2) ~ "1",
  TRUE ~ ComplAlimentation__1
))

## 2 - Elevages de plus de 100 poules (non bio)
# => "Aliments complets commercialisés importés" (3) à cocher dans l'alimentation

rga23 |>
  filter(NombrePoules1 > 100 | NombrePoules3 > 100) |>
  filter(ComplAlimentation__3 == 0) |>
  count()

rga23 <- rga23 |> mutate(ComplAlimentation__3 = case_when(
  (NombrePoules1 > 100 | NombrePoules3 > 100) ~ "1",
  TRUE ~ ComplAlimentation__3
))

## 3a - Elevages avec animaux qui ne consomment que
# - Céréales (2)
# - et/ou Aliments complets commercialisés importés (3)
# - et/ou Aliments complets commercialisés locaux (4)
# - et/ou Tourteau de coprah (5)
# => Aucune autonomie, tout est acheté

alimentsAchetesExclusivement <- (
  (rga23$ComplAlimentation__2 == 1 |
    rga23$ComplAlimentation__3 == 1 |
    rga23$ComplAlimentation__4 == 1 |
    rga23$ComplAlimentation__5 == 1) &
    rga23$ComplAlimentation__1 == 0 &
    rga23$ComplAlimentation__6 == 0 &
    rga23$ComplAlimentation__7 == 0 &
    rga23$ComplAlimentation__8 == 0 &
    rga23$ComplAlimentation__9 == 0 &
    rga23$ComplAlimentation__10 == 0)

rga23 |>
  filter(alimentsAchetesExclusivement) |>
  filter((!is.na(AutAlimAnimauxBasseCour) & AutAlimAnimauxBasseCour != 6) |
    (!is.na(AutAlimBovinsFourrage) & AutAlimBovinsFourrage != 6) |
    (!is.na(AutAlimCaprinsFourrage) & AutAlimCaprinsFourrage != 6) |
    (!is.na(AutAlimEquidesFourrages) & AutAlimEquidesFourrages != 6) |
    (!is.na(AutAlimOvinsFourrage) & AutAlimOvinsFourrage != 6) |
    (!is.na(AutAlimPorcins) & AutAlimPorcins != 6) |
    (!is.na(AutAlimPoules) & AutAlimPoules != 6)) |>
  select(interview__key, ComplAlimentation__2, ComplAlimentation__3, ComplAlimentation__4, ComplAlimentation__5, AutAlimAnimauxBasseCour, AutAlimBovinsFourrage, AutAlimCaprinsFourrage, AutAlimEquidesFourrages, AutAlimOvinsFourrage, AutAlimPorcins, AutAlimPoules)  |>
  count()

## 3b - Elevages avec animaux qui ne consomment que
# - Fourrage produit localement (1)
# - et/ou Ecarts de tri / déchets de culture (7)
# => Plus de 90% d'autonomie alimentaire

alimentsPresentsExclusivement <- (
  (rga23$ComplAlimentation__1 == 1 | rga23$ComplAlimentation__7 == 1) &
    (rga23$ComplAlimentation__2 == 0 &
      rga23$ComplAlimentation__3 == 0 &
      rga23$ComplAlimentation__4 == 0 &
      rga23$ComplAlimentation__5 == 0 &
      rga23$ComplAlimentation__6 == 0 &
      rga23$ComplAlimentation__8 == 0 &
      rga23$ComplAlimentation__9 == 0 &
      rga23$ComplAlimentation__10 == 0
    )
)

rga23 |>
  filter(alimentsPresentsExclusivement) |>
  filter((!is.na(AutAlimAnimauxBasseCour) & AutAlimAnimauxBasseCour != 1) |
    (!is.na(AutAlimBovinsFourrage) & AutAlimBovinsFourrage != 1) |
    (!is.na(AutAlimCaprinsFourrage) & AutAlimCaprinsFourrage != 1) |
    (!is.na(AutAlimEquidesFourrages) & AutAlimEquidesFourrages != 1) |
    (!is.na(AutAlimOvinsFourrage) & AutAlimOvinsFourrage != 1) |
    (!is.na(AutAlimPorcins) & AutAlimPorcins != 1) |
    (!is.na(AutAlimPoules) & AutAlimPoules != 1)) |>
  select(interview__key, ComplAlimentation__1, ComplAlimentation__7, AutAlimAnimauxBasseCour, AutAlimBovinsFourrage, AutAlimCaprinsFourrage, AutAlimEquidesFourrages, AutAlimOvinsFourrage, AutAlimPorcins, AutAlimPoules) |>
  count()

# AutAlimAnimauxBasseCour
# AutAlimBovinsFourrage
# AutAlimCaprinsFourrage
# AutAlimEquidesFourrages
# AutAlimOvinsFourrage
# AutAlimPorcins
# AutAlimPoules

rga23 <- rga23 |> mutate(
  AutAlimAnimauxBasseCour = case_when(
    !is.na(AutAlimAnimauxBasseCour) & alimentsPresentsExclusivement ~ "1",
    !is.na(AutAlimAnimauxBasseCour) & alimentsAchetesExclusivement ~ "6",
    TRUE ~ AutAlimAnimauxBasseCour
  ),
  AutAlimBovinsFourrage = case_when(
    !is.na(AutAlimBovinsFourrage) & alimentsPresentsExclusivement ~ "1",
    !is.na(AutAlimBovinsFourrage) & alimentsAchetesExclusivement ~ "6",
    TRUE ~ AutAlimBovinsFourrage
  ),
  AutAlimCaprinsFourrage = case_when(
    !is.na(AutAlimCaprinsFourrage) & alimentsPresentsExclusivement ~ "1",
    !is.na(AutAlimCaprinsFourrage) & alimentsAchetesExclusivement ~ "6",
    TRUE ~ AutAlimCaprinsFourrage
  ),
  AutAlimEquidesFourrages = case_when(
    !is.na(AutAlimEquidesFourrages) & alimentsPresentsExclusivement ~ "1",
    !is.na(AutAlimEquidesFourrages) & alimentsAchetesExclusivement ~ "6",
    TRUE ~ AutAlimEquidesFourrages
  ),
  AutAlimOvinsFourrage = case_when(
    !is.na(AutAlimOvinsFourrage) & alimentsPresentsExclusivement ~ "1",
    !is.na(AutAlimOvinsFourrage) & alimentsAchetesExclusivement ~ "6",
    TRUE ~ AutAlimOvinsFourrage
  ),
  AutAlimPorcins = case_when(
    !is.na(AutAlimPorcins) & alimentsPresentsExclusivement ~ "1",
    !is.na(AutAlimPorcins) & alimentsAchetesExclusivement ~ "6",
    TRUE ~ AutAlimPorcins
  ),
  AutAlimPoules = case_when(
    !is.na(AutAlimPoules) & alimentsPresentsExclusivement ~ "1",
    !is.na(AutAlimPoules) & alimentsAchetesExclusivement ~ "6",
    TRUE ~ AutAlimPoules
  )
)
