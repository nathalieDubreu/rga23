rga23 <- readCSV("rga23.csv")

valExtremes <- function(var, quantile) {
  rga23 |>
    filter({{ var }} > 0) |>
    select(interview__key, Archipel, {{ var }}) |>
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
valExtremes(NbRuchesPourProduire, 0.96)

rga23 |>
  filter(TotalAnes > 0)
# 1 seul exploitant

rga23 |>
  filter(NbDindesDindons > 0 | NbPintades > 0 | NbOies > 0 | NbCailles > 0 | NbAutresVolailles > 0)
# 0

