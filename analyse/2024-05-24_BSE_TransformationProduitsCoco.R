# Données transformations produits du cocotiers

## Champ : indicRGA23 == 1

rga23_champ <- inner_join(readCSV("rga23_mainOeuvre.csv") |> select(interview__key, Ile, starts_with("TransformationCoco")),
  readCSV("rga23_general.csv") |> filter(indicRGA23 == 1) |> select(interview__key, Archipel_1),
  by = "interview__key"
) |>
  left_join(readCSV("rga23_exploitations.csv") |> select(interview__key, IleExploitation),
    by = "interview__key"
  ) |>
  mutate(Ile = case_when(
    !is.na(IleExploitation) ~ IleExploitation,
    TRUE ~ Ile
  ))

## En quoi transformez-vous les produits du cocotier ? (C'est la transformation pour vente qui nous intéresse. Si non, cocher la dernière modalité.)
# Coprah..................................1/1
# Niau (tressage).........................2/2
# Fibres (bourre de coco).................3/3
# Huile vierge de coco....................4/4
# Lait de coco............................5/5
# Eau de coco.............................6/6
# Autre ..................................7/7
# Pas de vente des produits transformés...8/8

transformationsCocoPourVente <- paste0("TransformationCoco__", 1:7)

rga23_transfCoco <- rga23_champ |>
  mutate(
    nombreTransfPourVente = rowSums(across(
      all_of(transformationsCocoPourVente),
      ~ coalesce(., 0)
    )),
    nombreTransfPourVenteHorsCoprah = rowSums(across(
      all_of(transformationsCocoPourVente),
      ~ coalesce(., 0)
    )) - replace_na(TransformationCoco__1, 0)
  )

rga23_transfCoco_Archipel <- rga23_transfCoco |>
  group_by(Archipel_1) |>
  summarize(
    NombreExploitationsSeuilsRGA3 = n(),
    TransformationProduitsCocotiersPourVente = sum(ifelse(nombreTransfPourVente > 0, 1, 0), na.rm = TRUE),
    TransformationProduitsCocotiersPourVenteHorsCoprah = sum(ifelse(nombreTransfPourVenteHorsCoprah > 0, 1, 0), na.rm = TRUE)
  )

rga23_transfCoco_Total <- rga23_transfCoco |>
  mutate(Archipel_1 = "Polynésie Française") |>
  group_by(Archipel_1) |>
  summarize(
    NombreExploitationsSeuilsRGA3 = n(),
    TransformationProduitsCocotiersPourVente = sum(ifelse(nombreTransfPourVente > 0, 1, 0), na.rm = TRUE),
    TransformationProduitsCocotiersPourVenteHorsCoprah = sum(ifelse(nombreTransfPourVenteHorsCoprah > 0, 1, 0), na.rm = TRUE)
  )

BSE_rga23_transfCoco <- rbind(rga23_transfCoco_Archipel, rga23_transfCoco_Total)
writeCSV(BSE_rga23_transfCoco)
