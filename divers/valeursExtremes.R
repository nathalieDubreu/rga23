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

## Surfaces de jardins océaniens
valExtremes(SurfaceJardins, 0.96)

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

# Batiments et parcours

## Bovins
rga23_prodAnimales |>
  filter(CapaciteBatLogtBovins > 0 | SuperficieBatLogtBovins > 0) |>
  select(interview__key, CapaciteBatLogtBovins, SuperficieBatLogtBovins, nbTotalBovins)
# rga23_prodAnimales |> filter(CapaciteBatTraiteBovins>0)
# rga23_prodAnimales |> filter(SuperficieBatTraiteBovins>0)
# rga23_prodAnimales |> filter(CapaciteAutreBatBovins>0)
# rga23_prodAnimales |> filter(SuperficieAutreBatBovins>0)
rga23_prodAnimales |>
  filter(SurfacePaturageBovins > 0) |>
  select(interview__key, SurfacePaturageBovins, NbBovinsPaturage)

## Caprins
rga23_prodAnimales |>
  filter(CapaciteBatLogtCaprins > 0 | SuperficieBatLogtCaprins > 0) |>
  select(interview__key, CapaciteBatLogtCaprins, SuperficieBatLogtCaprins, nbTotalCaprins)
# rga23_prodAnimales |> filter(CapaciteBatTraiteCaprins>0)
# rga23_prodAnimales |> filter(SuperficieBatTraiteCaprins>0)
rga23_prodAnimales |>
  filter(CapaciteAutreBatCaprins > 0 | SuperficieAutreBatCaprins > 0) |>
  select(interview__key, CapaciteAutreBatCaprins, SuperficieAutreBatCaprins, nbTotalCaprins)
rga23_prodAnimales |>
  filter(SurfacePaturageCaprins > 0) |>
  select(interview__key, SurfacePaturageCaprins, NbCaprinsPaturage)

## Equides
rga23_prodAnimales |>
  filter(CapaciteBatLogtEquides > 0 | SuperficieBatLogtEquides > 0) |>
  select(interview__key, CapaciteBatLogtEquides, SuperficieBatLogtEquides, nbTotalEquides)
rga23_prodAnimales |>
  filter(CapaciteAutreBatEquides > 0 | SuperficieAutreBatEquides > 0) |>
  select(interview__key, CapaciteAutreBatEquides, SuperficieAutreBatEquides, nbTotalEquides)
rga23_prodAnimales |>
  filter(SurfacePaturageEquides > 0) |>
  select(interview__key, SurfacePaturageEquides, NbEquidesPaturage)

## Ovins
rga23_prodAnimales |>
  filter(CapaciteBatLogtOvins > 0 | SuperficieBatLogtOvins > 0) |>
  select(interview__key, CapaciteBatLogtOvins, SuperficieBatLogtOvins, nbTotalOvins)
# rga23_prodAnimales |> filter(CapaciteAutreBatOvins>0)
# rga23_prodAnimales |> filter(SuperficieAutreBatOvins>0)
rga23_prodAnimales |>
  filter(SurfacePaturageOvins > 0) |>
  select(interview__key, SurfacePaturageOvins, NbOvinsPaturage)

## Porcins
rga23_prodAnimales |>
  filter(CapaciteCabanesPorcs > 0 | SurfaceCabanesPorcs > 0) |>
  select(interview__key, CapaciteCabanesPorcs, SurfaceCabanesPorcs, nbTotalPorcs)
rga23_prodAnimales |>
  filter(CapaciteParcCochons > 0 | SurfaceParcCochons > 0) |>
  select(interview__key, CapaciteParcCochons, SurfaceParcCochons, nbTotalPorcs)
rga23_prodAnimales |>
  filter(CapacitePorcherieCaille > 0 | SurfacePorcherieCaille > 0) |>
  select(interview__key, CapacitePorcherieCaille, SurfacePorcherieCaille, nbTotalPorcs)
rga23_prodAnimales |>
  filter(CapacitePorcherieTrad > 0 | SurfacePorcherieTrad > 0) |>
  select(interview__key, CapacitePorcherieTrad, SurfacePorcherieTrad, nbTotalPorcs)
rga23_prodAnimales |>
  filter(SurfaceParcoursPorcins > 0) |>
  select(interview__key, SurfaceParcoursPorcins, nbTotalPorcins)

## Poules

### 0 - bio
rga23_prodAnimales |>
  filter(CapaciteMaxAutreBat0 > 0 | SurfaceAutreBat0 > 0 | SurfaceParcAutreBat0 > 0) |>
  select(interview__key, CapaciteMaxAutreBat0, SurfaceAutreBat0, SurfaceParcAutreBat0, NombrePoules0)
rga23_prodAnimales |>
  filter(CapaciteMaxBatSol0 > 0 | SurfaceBatSol0 > 0 | SurfaceParcBatSol0 > 0) |>
  select(interview__key, CapaciteMaxBatSol0, SurfaceBatSol0, SurfaceParcBatSol0, NombrePoules0)
rga23_prodAnimales |>
  filter(CapaciteMaxCabMob0 > 0 | SurfaceCabMob0 > 0 | SurfaceParcCabMob0 > 0) |>
  select(interview__key, CapaciteMaxCabMob0, SurfaceCabMob0, SurfaceParcCabMob0, NombrePoules0)

### Plein air ou au sol
rga23_prodAnimales |>
  filter(CapaciteMaxAutreBat1 > 0 | SurfaceAutreBat1 > 0 | SurfaceParcAutreBat1 > 0) |>
  select(interview__key, CapaciteMaxAutreBat1, SurfaceAutreBat1, SurfaceParcAutreBat1, NombrePoules1)
rga23_prodAnimales |>
  filter(CapaciteMaxBatSol1 > 0 | SurfaceBatSol1 > 0 | SurfaceParcBatSol1 > 0) |>
  select(interview__key, CapaciteMaxBatSol1, SurfaceBatSol1, SurfaceParcBatSol1, NombrePoules1)
rga23_prodAnimales |>
  filter(CapaciteMaxCabMob1 > 0 | SurfaceCabMob1 > 0 | SurfaceParcCabMob1 > 0) |>
  select(interview__key, CapaciteMaxCabMob1, SurfaceCabMob1, SurfaceParcCabMob1, NombrePoules1)

### 3 - En cage
rga23_prodAnimales |>
  filter(CapaciteMaxAutreBat3 > 0 | SurfaceAutreBat3 > 0 | CapaciteMaxBatCage3 > 0 | SurfaceBatCage3 > 0) |>
  select(interview__key, CapaciteMaxBatCage3, SurfaceBatCage3, CapaciteMaxAutreBat3, SurfaceAutreBat3, NombrePoules3)


## Volailles
rga23_prodAnimales |>
  filter(SuperficieBatDeplVolailles > 0 | SuperficieBatFixesVolailles > 0) |>
  select(
    interview__key, SuperficieBatDeplVolailles, SuperficieBatFixesVolailles,
    NbPoussins,
    NbPoulettes,
    NbPouletsChairCoqs,
    NbCanards,
    NbOies
  )
rga23_prodAnimales |>
  filter(SurfaceParcoursVolailles > 0) |>
  select(
    interview__key,
    SurfaceParcoursVolailles,
    NbPoussins,
    NbPoulettes,
    NbPouletsChairCoqs,
    NbCanards,
    NbOies
  )
