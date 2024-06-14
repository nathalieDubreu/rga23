library(purrr)
library(rlang)

# Autoconsommation.....................................1/1
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

calculSurfacesDestination <- function(partComVar, num, surfaceDestination, surfaceTypeCulture) {
  partComVarCol <- sym(paste0(partComVar, num))
  verifProdType <- sym(paste0(partComVar, "13"))
  surfaceTypeCulture <- sym(surfaceTypeCulture)

  result <- rga23_prod |>
    filter(!is.na(!!partComVarCol) & !is.na(!!surfaceTypeCulture) & !!verifProdType == 0) |>
    mutate(
      {{ surfaceDestination }} := !!partComVarCol * !!surfaceTypeCulture / 100
    ) |>
    rename(totalSurface = !!surfaceTypeCulture) |>
    select(interview__key, Archipel_1, Ile, Commune, {{ surfaceDestination }}, totalSurface)

  return(result)
}

genererPourcentagesParTypeCulture <- function(partTypeCulture, surfaceTypeCulture) {
  Modalite1 <- calculSurfacesDestination(partTypeCulture, 1, `Autoconsommation`, surfaceTypeCulture)
  Modalite2 <- calculSurfacesDestination(partTypeCulture, 2, `Alimentation des animaux`, surfaceTypeCulture)
  Modalite3 <- calculSurfacesDestination(partTypeCulture, 3, `Dons (à la famille ou à des amis)`, surfaceTypeCulture)
  Modalite4 <- calculSurfacesDestination(partTypeCulture, 4, `Echange`, surfaceTypeCulture)
  Modalite5 <- calculSurfacesDestination(partTypeCulture, 5, `Vente directe au particulier`, surfaceTypeCulture)
  Modalite6 <- calculSurfacesDestination(partTypeCulture, 6, `Vente par internet (Facebook ou autre site)`, surfaceTypeCulture)
  Modalite7 <- calculSurfacesDestination(partTypeCulture, 7, `Vente à un commerçant ou artisan ou revendeur`, surfaceTypeCulture)
  Modalite8 <- calculSurfacesDestination(partTypeCulture, 8, `Vente à un grossiste`, surfaceTypeCulture)
  Modalite9 <- calculSurfacesDestination(partTypeCulture, 9, `Vente à un transformateur ou préparateur`, surfaceTypeCulture)
  Modalite10 <- calculSurfacesDestination(partTypeCulture, 10, `Vente à la coopérative ou au syndicat`, surfaceTypeCulture)
  Modalite11 <- calculSurfacesDestination(partTypeCulture, 11, `Vente à la restauration collective`, surfaceTypeCulture)
  Modalite12 <- calculSurfacesDestination(partTypeCulture, 12, `Vente aux restaurants / hôtels`, surfaceTypeCulture)
  listeTables <- mget(nomsTables)
  TCD <- reduce(listeTables, left_join, by = c("interview__key", "Archipel_1", "Ile", "Commune", "totalSurface"))
  return(TCD)
}

nomsTables <- paste0("Modalite", 1:12)

## Cultures
rga23_prod <- left_join(rga23_champ_Ile_Commune,
  readCSV("rga23_prodVegetales.csv"),
  by = "interview__key"
)
TCD34 <- genererPourcentagesParTypeCulture("PartComMaraic__", "totalSurfaceMarai")
writeCSV(TCD34)
TCD35 <- genererPourcentagesParTypeCulture("PartComVivri__", "totalSurfaceVivri")
writeCSV(TCD35)
TCD36 <- genererPourcentagesParTypeCulture("PartComFruit__", "totalSurfaceFruit")
writeCSV(TCD36)
TCD37 <- genererPourcentagesParTypeCulture("PartComPepinieres__", "totalSurfacePepinieres")
writeCSV(TCD37)
TCD38 <- genererPourcentagesParTypeCulture("PartComPlantes__", "totalSurfacePlantes")
writeCSV(TCD38)
TCD39 <- genererPourcentagesParTypeCulture("PartComFlorale__", "totalSurfaceFlorale")
writeCSV(TCD39)
TCD40 <- genererPourcentagesParTypeCulture("PartComFourrages__", "totalSurfaceFourrages")
writeCSV(TCD40)
