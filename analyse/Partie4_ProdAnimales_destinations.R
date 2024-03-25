#### Regroupement de modalités de destination des produits
rga23_prodAnimales_regroupements <- rga23_prodAnimales |>
  mutate(
    ## Regroupements des destinations pour les Oeufs
    PartComOeufs__1_4 = rowSums(across(
      all_of(paste0("PartComOeufs__", 1:4)),
      ~ coalesce(., 0)
    )),
    PartComOeufs__5_6 = rowSums(across(
      all_of(paste0("PartComOeufs__", 5:6)),
      ~ coalesce(., 0)
    )),
    PartComOeufs__7_12 = rowSums(across(
      all_of(paste0("PartComOeufs__", 7:12)),
      ~ coalesce(., 0)
    )),
    NbOeufs = replace_na(ProductionPoules0, 0) + replace_na(ProductionPoules1, 0) + replace_na(ProductionPoules3, 0),
    NbOeufs_DHV = NbOeufs * PartComOeufs__1_4 / 100,
    NbOeufs_VD = NbOeufs * PartComOeufs__5_6 / 100,
    NbOeufs_VP = NbOeufs * PartComOeufs__7_12 / 100,
    NbOeufsAutresPoules = replace_na(ProductionPoules0, 0) + replace_na(ProductionPoules1, 0),
    NbOeufsAutresPoules_DHV = NbOeufsAutresPoules * PartComOeufs__1_4 / 100,
    NbOeufsAutresPoules_VD = NbOeufsAutresPoules * PartComOeufs__5_6 / 100,
    NbOeufsAutresPoules_VP = NbOeufsAutresPoules * PartComOeufs__7_12 / 100,
    NbOeufsPoulesEnCage = replace_na(ProductionPoules3, 0),
    NbOeufsPoulesEnCage_DHV = NbOeufsPoulesEnCage * PartComOeufs__1_4 / 100,
    NbOeufsPoulesEnCage_VD = NbOeufsPoulesEnCage * PartComOeufs__5_6 / 100,
    NbOeufsPoulesEnCage_VP = NbOeufsPoulesEnCage * PartComOeufs__7_12 / 100,
    ## Regroupements des destinations pour le Miel
    PartComMiel__1_4 = rowSums(across(
      all_of(paste0("PartComMiel__", 1:4)),
      ~ coalesce(., 0)
    )),
    PartComMiel__5_6 = rowSums(across(
      all_of(paste0("PartComMiel__", 5:6)),
      ~ coalesce(., 0)
    )),
    PartComMiel__7_12 = rowSums(across(
      all_of(paste0("PartComMiel__", 7:12)),
      ~ coalesce(., 0)
    )),
    KilosMiel_DHV = replace_na(ProductionRuches, 0) * PartComMiel__1_4 / 100,
    KilosMiel_VD = replace_na(ProductionRuches, 0) * PartComMiel__5_6 / 100,
    KilosMiel_VP = replace_na(ProductionRuches, 0) * PartComMiel__7_12 / 100,
    # ## Regroupements des destinations pour la Viande
    # PartComViande__1_4 = rowSums(across(
    #   all_of(paste0("PartComViande__", 1:4)),
    #   ~ coalesce(., 0)
    # )),
    # PartComViande__5_6 = rowSums(across(
    #   all_of(paste0("PartComViande__", 5:6)),
    #   ~ coalesce(., 0)
    # )),
    # PartComViande__7_12 = rowSums(across(
    #   all_of(paste0("PartComViande__", 7:12)),
    #   ~ coalesce(., 0)
    # ))
  )

Partie4_destinationOeufsEtMiel <- rga23_prodAnimales_regroupements |> summarize(
  ## Tous les oeufs
  NbOeufs_Total = sum(NbOeufs),
  partOeufs_DHV = round(sum(NbOeufs_DHV) / (sum(NbOeufs_DHV) + sum(NbOeufs_VD) + sum(NbOeufs_VP)) * 100),
  partOeufs_VD = round(sum(NbOeufs_VD) / (sum(NbOeufs_DHV) + sum(NbOeufs_VD) + sum(NbOeufs_VP)) * 100),
  partOeufs_VP = round(sum(NbOeufs_VP) / (sum(NbOeufs_DHV) + sum(NbOeufs_VD) + sum(NbOeufs_VP)) * 100),
  ## Oeufs des poules pas en cage
  NbOeufsAutresPoules_Total = sum(NbOeufsAutresPoules),
  partOeufsAutresPoules_DHV = round(sum(NbOeufsAutresPoules_DHV) / (sum(NbOeufsAutresPoules_DHV) + sum(NbOeufsAutresPoules_VD) + sum(NbOeufsAutresPoules_VP)) * 100),
  partOeufsAutresPoules_VD = round(sum(NbOeufsAutresPoules_VD) / (sum(NbOeufsAutresPoules_DHV) + sum(NbOeufsAutresPoules_VD) + sum(NbOeufsAutresPoules_VP)) * 100),
  partOeufsAutresPoules_VP = round(sum(NbOeufsAutresPoules_VP) / (sum(NbOeufsAutresPoules_DHV) + sum(NbOeufsAutresPoules_VD) + sum(NbOeufsAutresPoules_VP)) * 100),
  ## Oeufs des poules en cage
  NbOeufsPoulesEnCage_Total = sum(NbOeufsPoulesEnCage),
  partOeufsPoulesEnCage_DHV = round(sum(NbOeufsPoulesEnCage_DHV) / (sum(NbOeufsPoulesEnCage_DHV) + sum(NbOeufsPoulesEnCage_VD) + sum(NbOeufsPoulesEnCage_VP)) * 100),
  partOeufsPoulesEnCage_VD = round(sum(NbOeufsPoulesEnCage_VD) / (sum(NbOeufsPoulesEnCage_DHV) + sum(NbOeufsPoulesEnCage_VD) + sum(NbOeufsPoulesEnCage_VP)) * 100),
  partOeufsPoulesEnCage_VP = round(sum(NbOeufsPoulesEnCage_VP) / (sum(NbOeufsPoulesEnCage_DHV) + sum(NbOeufsPoulesEnCage_VD) + sum(NbOeufsPoulesEnCage_VP)) * 100),
  ## Miel
  KilosMiel_Total = sum(replace_na(ProductionRuches, 0)),
  partMiel_DHV = round(sum(KilosMiel_DHV) / (sum(KilosMiel_DHV) + sum(KilosMiel_VD) + sum(KilosMiel_VP)) * 100),
  partMiel_VD = round(sum(KilosMiel_VD) / (sum(KilosMiel_DHV) + sum(KilosMiel_VD) + sum(KilosMiel_VP)) * 100),
  partMiel_VP = round(sum(KilosMiel_VP) / (sum(KilosMiel_DHV) + sum(KilosMiel_VD) + sum(KilosMiel_VP)) * 100)
)
writeCSV(Partie4_destinationOeufsEtMiel)
