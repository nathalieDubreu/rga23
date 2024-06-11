# Typologie basée sur la culture de la vanille

## Etape préalable vanille

vanillerOmbrageNaturel <- readCSV("rga23_surfacesCultures.csv") |>
  filter(culture_id == 509) |>
  mutate(
    PointsVanilleOmbrageNaturel = SurfaceCult * 3.1
  ) |>
  select(interview__key, PointsVanilleOmbrageNaturel)

vanillerSerreOmbriere <- readCSV("rga23_surfacesCultures.csv") |>
  filter(culture_id == 510) |>
  mutate(
    PointsVanilleSerreOmbriere = SurfaceCult * 3.1
  ) |>
  select(interview__key, PointsVanilleSerreOmbriere)

vanille <- full_join(vanillerOmbrageNaturel, vanillerSerreOmbriere, by = c("interview__key")) |>
  mutate(PointsVanille = replace_na(PointsVanilleOmbrageNaturel, 0) +
    replace_na(PointsVanilleSerreOmbriere, 0)) |>
  mutate(across(where(is.numeric), ~ coalesce(., 0)))

## Définition de la table à merger (variable à ajouter)
tableAMerger <- left_join(rga23_champ_Ile_Commune, vanille, by = "interview__key") |>
  left_join(
    readCSV("rga23_general.csv") |> select(interview__key, PointsCAPL),
    by = "interview__key"
  ) |>
  mutate(TypeExploit = case_when(
    PointsVanille / PointsCAPL >= 0.7 & PointsVanilleSerreOmbriere >= PointsVanilleOmbrageNaturel ~ "a - Vanille (principalement sous serre ou ombrière) pour au moins 70% des points CAPL",
    PointsVanille / PointsCAPL >= 0.7 & PointsVanilleOmbrageNaturel > PointsVanilleSerreOmbriere ~ "b - Vanille (principalement sous ombrage naturel) pour au moins 70% des points CAPL",
    PointsVanille / PointsCAPL < 0.7 ~ "c - Vanille présente mais représentant moins de 70% des points CAPL",
    is.na(PointsVanille) ~ "d - Pas de vanille",
    TRUE ~ "AUTRE"
  )) |>
  select(interview__key, TypeExploit)

tableAMerger |>
  group_by(TypeExploit) |>
  count()

## Suffixe correspondant
suffixeNomTable <- "_4_Vanille"

## Lancement du programme d'ajout de la colonne et d'export des différents CSV pour la constrution des TCD
source("TCD/AjoutColonneTypeExploitEtExportsCSV.R")
