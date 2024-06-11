# Typologie basée sur la présence de pâturages

## Définir la table à merger (variable à ajouter)
tableAMerger <- left_join(
  rga23_champ_Ile_Commune |> select(interview__key, Cultivateurs, Eleveurs, ProducteursCoprah),
  readCSV("rga23_prodVegetales.csv") |> select(interview__key, SurfaceProdVegetales_HC_HP, TypeFourrages__701, TypeFourrages__702, TypeFourrages__703),
  by = "interview__key"
) |>
  mutate(TypeExploit = case_when(
    (TypeFourrages__701 == 1 | TypeFourrages__702 == 1 | TypeFourrages__703 == 1) &
      Eleveurs == 0 ~ "a - Pâturages sans élevages déclarés",
    (TypeFourrages__701 == 1 | TypeFourrages__702 == 1 | TypeFourrages__703 == 1) &
      Eleveurs == 1 & SurfaceProdVegetales_HC_HP > 0 ~ "b - Pâturages et autres productions végétales (HC) avec élevages déclarés",
    (TypeFourrages__701 == 1 | TypeFourrages__702 == 1 | TypeFourrages__703 == 1) &
      Eleveurs == 1 & (is.na(SurfaceProdVegetales_HC_HP) | SurfaceProdVegetales_HC_HP == 0) ~ "c - Pâturages sans autres productions végétales (HC) avec élevages déclarés",
    (TypeFourrages__701 == 0 | is.na(TypeFourrages__701)) &
      (TypeFourrages__702 == 0 | is.na(TypeFourrages__702)) &
      (TypeFourrages__703 == 0 | is.na(TypeFourrages__703)) &
      Eleveurs == 1 ~ "d - Eleveurs sans pâturages",
    (TypeFourrages__701 == 0 | is.na(TypeFourrages__701)) &
      (TypeFourrages__702 == 0 | is.na(TypeFourrages__702)) &
      (TypeFourrages__703 == 0 | is.na(TypeFourrages__703)) &
      Eleveurs == 0 &
      (Cultivateurs == 1 | ProducteursCoprah == 1) ~ "e - Cultivateurs ou producteurs de coprah sans pâturages ni élevages",
    TRUE ~ "AUTRE"
  )) |>
  select(interview__key, TypeExploit)

tableAMerger |>
  group_by(TypeExploit) |>
  count()

## Suffixe correspondant
suffixeNomTable <- "_Paturages"

## Lancement du programme d'ajout de la colonne et d'export des différents CSV pour la constrution des TCD
source("TCD/AjoutColonneTypeExploitEtExportsCSV.R")
