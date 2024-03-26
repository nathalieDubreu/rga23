# Vente
# Auto-consommation familiale.....................................1/1
# Alimentation des animaux .......................................2/2
# Dons (à la famille, des amis)...................................3/3
# Echange.........................................................4/4
# Vente directe au particulier ...................................5/5
# Vente par internet (Facebook ou autre site).....................6/6
# Vente à un commerçant, artisan ou revendeur.....................7/7
# Vente à un grossiste............................................8/8
# Vente à un transformateur ou préparateur (y compris abattoir)...9/9
# Vente à la coopérative ou au syndicat...........................10/10
# Vente à la restauration collective..............................11/11
# Vente aux restaurants (hors collectifs) / hôtels................12/12
# Sans objet (pas de production de ce type).......................13/13

nbCulturesParType <- rga23_surfacesCultures |>
  group_by(interview__key, TypeCulture) |>
  summarize(nbCultures = n()) |>
  pivot_wider(names_from = TypeCulture, values_from = nbCultures, values_fill = 0)

rga23_venteVegetales <- left_join(rga23_prodVegetales, nbCulturesParType, by = "interview__key") |>
  mutate(
    partVendueMaraic = rowSums(across(
      all_of(paste0("PartComMaraic__", 5:12)),
      ~ coalesce(., 0)
    )), partVendueVivri = rowSums(across(
      all_of(paste0("PartComVivri__", 5:12)),
      ~ coalesce(., 0)
    )), partVendueFruit = rowSums(across(
      all_of(paste0("PartComFruit__", 5:12)),
      ~ coalesce(., 0)
    )), partVenduePlantes = rowSums(across(
      all_of(paste0("PartComPlantes__", 5:12)),
      ~ coalesce(., 0)
    )), partVendueFlorale = rowSums(across(
      all_of(paste0("PartComFlorale__", 5:12)),
      ~ coalesce(., 0)
    )), partVenduePepinieres = rowSums(across(
      all_of(paste0("PartComPepinieres__", 5:12)),
      ~ coalesce(., 0)
    )), partVendueFourrages = rowSums(across(
      all_of(paste0("PartComFourrages__", 5:12)),
      ~ coalesce(., 0)
    )),
    venteProduitsVegetaux = ifelse(partVendueMaraic > 0, 1, 0) +
      ifelse(partVendueVivri > 0, 1, 0) +
      ifelse(partVendueFruit > 0, 1, 0) +
      ifelse(partVendueFlorale > 0, 1, 0) +
      ifelse(partVenduePlantes > 0, 1, 0) +
      ifelse(partVenduePepinieres > 0, 1, 0) +
      ifelse(partVendueFourrages > 0, 1, 0),
    nbProduitsVegetauxVendus = ifelse(partVendueMaraic > 0, `10`, 0) +
      ifelse(partVendueVivri > 0, `20`, 0) +
      ifelse(partVendueFruit > 0, `30`, 0) +
      ifelse(partVendueFlorale > 0, `40`, 0) +
      ifelse(partVenduePlantes > 0, `50`, 0) +
      ifelse(partVenduePepinieres > 0, `60`, 0) +
      ifelse(partVendueFourrages > 0, `70`, 0)
  ) |>
  select(interview__key, partVendueMaraic, partVendueVivri, partVendueFruit, partVenduePlantes, partVendueFlorale, partVenduePepinieres, partVendueFourrages, venteProduitsVegetaux, nbProduitsVegetauxVendus)

rga23_venteAnimales <- rga23_prodAnimales |>
  mutate(
    partVendueOeufs = rowSums(across(
      all_of(paste0("PartComOeufs__", 5:12)),
      ~ coalesce(., 0)
    )), partVendueMiel = rowSums(across(
      all_of(paste0("PartComMiel__", 5:12)),
      ~ coalesce(., 0)
    )), partVendueViande = rowSums(across(
      all_of(paste0("PartComViande__", 5:12)),
      ~ coalesce(., 0)
    )),
    venteProduitsAnimaux = ifelse(partVendueOeufs > 0, 1, 0) +
      ifelse(partVendueMiel > 0, 1, 0) +
      ifelse(partVendueViande > 0, 1, 0)
  ) |>
  select(interview__key, partVendueOeufs, partVendueMiel, partVendueViande, venteProduitsAnimaux)

rga23_tapeAvecVentes <- left_join(
  left_join(rga23_tape,
            rga23_venteAnimales,
            by = c("interview__key")
  ),
  rga23_venteVegetales,
  by = c("interview__key")
) |>
  mutate(
    venteTypeProduits = ifelse(is.na(venteProduitsAnimaux), 0, venteProduitsAnimaux) + ifelse(is.na(venteProduitsVegetaux), 0, venteProduitsVegetaux),
    venteNbProduits = ifelse(is.na(venteProduitsAnimaux), 0, venteProduitsAnimaux) + ifelse(is.na(nbProduitsVegetauxVendus), 0, nbProduitsVegetauxVendus)
  )