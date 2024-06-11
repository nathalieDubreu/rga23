# Typologie basée sur [TODO : complèter]

## Définir la table à merger (variable à ajouter)
tableAMerger <- left_join(
  rga23_champ_Ile_Commune |> select(interview__key, Cultivateurs, Eleveurs, ProducteursCoprah),
  readCSV("rga23_tousFichiersPlats.csv"),
  by = "interview__key"
) |>
  mutate(TypeExploit = case_when(
    # [TODO alimenter la variable de typologie - en faisant les bons left_join...]
    TRUE ~ "AUTRE"
  )) |>
  select(interview__key, TypeExploit)

tableAMerger |>
  group_by(TypeExploit) |>
  count()

## Suffixe correspondant [TODO : maj]
suffixeNomTable <- "_NouvelleTypologie"

## Lancement du programme d'ajout de la colonne et d'export des différents CSV pour la constrution des TCD
source("TCD/AjoutColonneTypeExploitEtExportsCSV.R")
