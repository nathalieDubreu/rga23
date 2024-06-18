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

## Fonction qui permet d'attribuer une part à la destination du type de produits (ex : Autoconsommation de 0 à 25%)
calculPartsDestination <- function(partComVar, num, destinationVar) {
  partComVarCol <- sym(paste0(partComVar, num))
  verifProdType <- sym(paste0(partComVar, "13"))

  result <- rga23_prod |>
    filter(!is.na(!!partComVarCol) & !!verifProdType == 0) |>
    mutate(
      {{ destinationVar }} := case_when(
        !!partComVarCol <= 25 ~ "0 à 25%",
        !!partComVarCol <= 50 ~ "25 et 50%",
        !!partComVarCol <= 75 ~ "50 et 75%",
        !!partComVarCol <= 100 ~ "Plus de 75%",
        TRUE ~ "???"
      )
    ) |>
    select(interview__key, Archipel_1, Ile, Commune, {{ destinationVar }}) |>
    mutate(dummy = 1) |>
    spread(key = {{ destinationVar }}, value = dummy, fill = 0, sep = " : ")

  return(result)
}

## Fonction de lancement du calcul des parts pour les 12 destinations possibles et mise en forme en un fichier à exporter
genererModalitesParTypeCulture <- function(partTypeCulture) {
  Modalite1 <- calculPartsDestination(partTypeCulture, 1, `Autoconsommation`)
  Modalite2 <- calculPartsDestination(partTypeCulture, 2, `Alimentation des animaux`)
  Modalite3 <- calculPartsDestination(partTypeCulture, 3, `Dons (à la famille ou à des amis)`)
  Modalite4 <- calculPartsDestination(partTypeCulture, 4, `Echange`)
  Modalite5 <- calculPartsDestination(partTypeCulture, 5, `Vente directe au particulier`)
  Modalite6 <- calculPartsDestination(partTypeCulture, 6, `Vente par internet (Facebook ou autre site)`)
  Modalite7 <- calculPartsDestination(partTypeCulture, 7, `Vente à un commerçant ou artisan ou revendeur`)
  Modalite8 <- calculPartsDestination(partTypeCulture, 8, `Vente à un grossiste`)
  Modalite9 <- calculPartsDestination(partTypeCulture, 9, `Vente à un transformateur ou préparateur (y compris abattoir)`)
  Modalite10 <- calculPartsDestination(partTypeCulture, 10, `Vente à la coopérative ou au syndicat`)
  Modalite11 <- calculPartsDestination(partTypeCulture, 11, `Vente à la restauration collective`)
  Modalite12 <- calculPartsDestination(partTypeCulture, 12, `Vente aux restaurants (hors collectifs) / hôtels`)
  listeTables <- mget(nomsTables)
  TCD <- reduce(listeTables, left_join, by = c("interview__key", "Archipel_1", "Ile", "Commune"))
  return(TCD)
}

nomsTables <- paste0("Modalite", 1:12)

## Cultures
rga23_prod <- left_join(rga23_champ_Ile_Commune,
                        readCSV("rga23_prodVegetales.csv"),
                        by = "interview__key"
)
TCD24 <- genererModalitesParTypeCulture("PartComMaraic__")
writeCSV(TCD24)
TCD25 <- genererModalitesParTypeCulture("PartComVivri__")
writeCSV(TCD25)
TCD26 <- genererModalitesParTypeCulture("PartComFruit__")
writeCSV(TCD26)
TCD27 <- genererModalitesParTypeCulture("PartComPepinieres__")
writeCSV(TCD27)
TCD28 <- genererModalitesParTypeCulture("PartComPlantes__")
writeCSV(TCD28)
TCD29 <- genererModalitesParTypeCulture("PartComFlorale__")
writeCSV(TCD29)
TCD30 <- genererModalitesParTypeCulture("PartComFourrages__")
writeCSV(TCD30)

## Animaux
rga23_prod <- left_join(rga23_champ_Ile_Commune,
                        readCSV("rga23_prodAnimales.csv"),
                        by = "interview__key"
)
TCD31 <- genererModalitesParTypeCulture("PartComOeufs__")
writeCSV(TCD31)
TCD32 <- genererModalitesParTypeCulture("PartComMiel__")
writeCSV(TCD32)
TCD33 <- genererModalitesParTypeCulture("PartComViande__")
writeCSV(TCD33)
