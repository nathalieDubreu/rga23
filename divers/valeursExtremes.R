rga23 <- readCSV("rga23.csv")

valExtremes <- function(var, quantile) {
  rga23 |>
    filter({{ var }} > 0) |>
    select(interview__key, Archipel, {{ var }}, interview__status) |>
    filter({{ var }} > quantile({{ var }}, probs = c(quantile))) |>
    arrange(desc({{ var }}))
}

## Main d'oeuvre
valExtremes(NbCoExploitants, 0.99)
valExtremes(NbMOPermFamiliale, 0.98)
valExtremes(nbFemmesNFPerm, 0.93)
valExtremes(nbHommesNFPerm, 0.98)
rga23 <- rga23 |>
  mutate(totalMAOccas = ifelse(is.na(NbFemOccasAvecLien), 0, NbFemOccasAvecLien) +
    ifelse(is.na(NbFemOccasSansLien), 0, NbFemOccasSansLien) +
    ifelse(is.na(NbHomOccasAvecLien), 0, NbHomOccasAvecLien) +
    ifelse(is.na(NbHomOccasSansLien), 0, NbHomOccasSansLien))
valExtremes(totalMAOccas, 0.98)

## Surfaces les plus importantes par type de cultures
valExtremes(totalSurfaceMarai, 0.99)
valExtremes(totalSurfacePepinieres, 0.95)
valExtremes(totalSurfaceFruit, 0.992)
valExtremes(totalSurfaceVivri, 0.99)
valExtremes(totalSurfacePlantes, 0.99)
valExtremes(totalSurfaceFourrages, 0.97)
valExtremes(totalSurfaceJacheres, 0.99)
valExtremes(totalSurfaceFlorale, 0.99)

## Cheptels les plus importants
valExtremes(nbTotalBovins, 0.95)
valExtremes(nbTotalEquides, 0.9)
valExtremes(nbTotalCaprins, 0.95)
valExtremes(nbTotalPorcs, 0.97)
valExtremes(NombrePoules3, 0.9)
valExtremes(NombrePoules1, 0.9)
valExtremes(NbPoussins, 0.7)
valExtremes(NbPoulettes, 0.7)
valExtremes(NbPouletsChairCoqs, 0.7)
valExtremes(NbCanards, 0.7)
valExtremes(NbOies, 0.1)
valExtremes(NbRuchesPourProduire, 0.96)

rga23 |>
  filter(TotalAnes > 0)
# 1 seul exploitant

rga23 |>
  filter(NbDindesDindons > 0 | NbPintades > 0 | NbCailles > 0 | NbAutresVolailles > 0)
# 0

# Auto-consommation > 90% - cultures
rga23_prodVegetales <- readCSV("rga23_prodVegetales.csv")
autoConsoGrandesSurfaces <- rga23_prodVegetales |>
  filter((PartComMaraic__1 > 90 & totalSurfaceMarai > 500) |
    (PartComFruit__1 > 90 & totalSurfaceFruit > 10000) |
    (PartComVivri__1 > 90 & totalSurfaceVivri >= 2500) |
    (PartComFlorale__1 > 90 & totalSurfaceFlorale >= 2500) |
    (PartComPlantes__1 > 90 & totalSurfacePlantes >= 2500) |
    (PartComPepinieres__1 > 90 & totalSurfacePepinieres >= 500)) |>
  select(interview__key, SurfaceTotalProdAgri, totalSurfaceMarai, PartComMaraic__1, totalSurfaceFruit, PartComFruit__1, totalSurfaceVivri, PartComVivri__1, totalSurfaceFlorale, PartComFlorale__1, totalSurfacePlantes, PartComPlantes__1, totalSurfacePepinieres, PartComPepinieres__1)
# writeCSV(autoConsoGrandesSurfaces)

# Auto-consommation > 90% - productions animales
rga23_prodAnimales <- readCSV("rga23_prodAnimales.csv")

## Oeufs
rga23_prodAnimales |>
  filter(PartComOeufs__1 > 90 & (ifelse(is.na(ProductionPoules0), 0, ProductionPoules0) + ifelse(is.na(ProductionPoules1), 0, ProductionPoules1) + ifelse(is.na(ProductionPoules3), 0, ProductionPoules3)) > 1000) |>
  select(interview__key, ProductionPoules3, ProductionPoules1, ProductionPoules0, PartComOeufs__1)

## Miel
rga23_prodAnimales |>
  filter(PartComMiel__1 > 90 & ProductionRuches > 10) |>
  select(interview__key, ProductionRuches, PartComMiel__1)

## Viande (bovins, équidés, porc et caprins)
rga23_prodAnimales |>
  filter(PartComViande__1 == 100 &
    (ifelse(is.na(nbTotalBovins), 0, nbTotalBovins) +
      ifelse(is.na(nbTotalEquides), 0, nbTotalEquides) +
      ifelse(is.na(nbTotalCaprins), 0, nbTotalCaprins) +
      ifelse(is.na(nbTotalPorcs), 0, nbTotalPorcs)) > 50) |>
  select(interview__key, nbTotalBovins, nbTotalCaprins, nbTotalPorcs)

# Valeurs extrèmes productions ruches et poules
valExtremes(ProductionRuches, 0.98)
valExtremes(ProductionPoules1, 0.95)
valExtremes(ProductionPoules3, 0.95)
