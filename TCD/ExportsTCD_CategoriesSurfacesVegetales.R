# Typologie basée sur la surface de cultures végétales (hors pâturages et hors cocoteraies)

## Définir la table à merger (variable à ajouter)
tableAMerger <- left_join(
  rga23_champ_Ile_Commune |> select(interview__key, Cultivateurs, Eleveurs, ProducteursCoprah),
  full_join(
    readCSV("rga23_prodVegetales.csv") |>
      select(interview__key, SurfaceJardins),
    readCSV("rga23_surfacesCultures.csv") |>
      filter(culture_id != 701 & culture_id != 702 & culture_id != 705 & culture_id != 307 & culture_id != 308 & culture_id != 309) |>
      group_by(interview__key) |>
      summarize(surface = sum(SurfaceCult, 0)),
    by = "interview__key"
  ) |>
    mutate(SurfaceCultVegetales = replace_na(surface, 0) + replace_na(SurfaceJardins, 0)) |>
    select(interview__key, SurfaceCultVegetales),
  by = "interview__key"
) |>
  mutate(TypeExploit = case_when(
    is.na(SurfaceCultVegetales) ~ "a - Aucune surface de cultures végétales",
    SurfaceCultVegetales == 0 ~ "a - Aucune surface de cultures végétales",
    SurfaceCultVegetales < 5000 ~ "b - Moins de 5000m2 de cultures végétales",
    SurfaceCultVegetales < 10000 ~ "c - Entre 5000m2 et 1Ha de cultures végétales",
    SurfaceCultVegetales >= 10000 ~ "d - 1Ha ou plus de cultures végétales",
  )) |>
  select(interview__key, TypeExploit)

tableAMerger |>
  group_by(TypeExploit) |>
  count()

## Suffixe correspondant
suffixeNomTable <- "_CategorieSurfaceCultVege_HC_HP"

## Lancement du programme d'ajout de la colonne et d'export des différents CSV pour la constrution des TCD
source("TCD/AjoutColonneTypeExploitEtExportsCSV.R")
