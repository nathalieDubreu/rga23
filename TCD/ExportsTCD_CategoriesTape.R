# Typologie basée sur les catégories inspirées de Tape

## Définir la table à merger (variable à ajouter)
tableAMerger <- left_join(
  rga23_champ_Ile_Commune |> select(interview__key, Cultivateurs, Eleveurs, ProducteursCoprah),
  readCSV("rga23_prodVegetales.csv") |> select(interview__key, SurfaceTotalProdAgri, totalSurfaceMarai, totalSurfaceFruit, totalSurfaceVivri, totalSurfacePlantes),
  by = "interview__key"
) |>
  mutate(TypeExploit = case_when(
    Eleveurs == 0 & totalSurfaceMarai / SurfaceTotalProdAgri >= 2 / 3 ~ "a - Maraichage sur plus de 2/3 de la SAU déclarée SANS élevage",
    Eleveurs == 0 & totalSurfaceVivri / SurfaceTotalProdAgri >= 2 / 3 ~ "b - Vivriers sur plus de 2/3 de la SAU déclarée SANS élevage",
    Eleveurs == 0 & totalSurfaceFruit / SurfaceTotalProdAgri >= 2 / 3 ~ "c - Fruitiers sur plus de 2/3 de la SAU déclarée SANS élevage",
    Eleveurs == 0 & totalSurfacePlantes / SurfaceTotalProdAgri >= 2 / 3 ~ "d - Plantes aromatiques, stimulantes et médicinales sur plus de 2/3 de la SAU déclarée SANS élevage",
    Eleveurs == 0 & Cultivateurs == 1 ~ "e - Cultures (y compris production éventuelle de coprah) SANS élevage",
    Eleveurs == 1 & Cultivateurs == 1 ~ "f - Cultures (y compris production éventuelle de coprah) ET élevage",
    Eleveurs == 1 & Cultivateurs == 0 & ProducteursCoprah == 0 ~ "g - Uniquement élevage sans pâturages. Pas de cultures ni production de coprah",
    Cultivateurs == 0 & ProducteursCoprah == 1 ~ "h - Pas de cultures. Uniquement production de coprah et éventuellement élevage sans pâturages",
    TRUE ~ "AUTRE"
  )) |>
  select(interview__key, TypeExploit)

tableAMerger |>
  group_by(TypeExploit) |>
  count()

## Suffixe correspondant
suffixeNomTable <- "_3_CategoriesTape"

## Lancement du programme d'ajout de la colonne et d'export des différents CSV pour la constrution des TCD
source("TCD/AjoutColonneTypeExploitEtExportsCSV.R")
