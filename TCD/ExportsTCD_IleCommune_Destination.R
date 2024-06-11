library(purrr)

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

calculPartsDestination <- function(partComVar, destinationVar) {
  result <- rga23_prod |>
    filter(!is.na({{ partComVar }})) |>
    mutate(
      {{ destinationVar }} := case_when(
        {{ partComVar }} <= 25 ~ "0 à 25%",
        {{ partComVar }} <= 50 ~ "25 et 50%",
        {{ partComVar }} <= 75 ~ "50 et 75%",
        {{ partComVar }} <= 100 ~ "Plus de 75%",
        TRUE ~ "???"
      )
    ) |> 
    select(interview__key, Archipel_1, Ile, Commune, {{ destinationVar }}) |>
    mutate(dummy = 1) |>
    spread(key = {{ destinationVar }}, value = dummy, fill = 0, sep = " : ")

  return(result)
}

rga23_prod <- left_join(rga23_champ_Ile_Commune,
                        readCSV("rga23_prodVegetales.csv"),
                        by = "interview__key")

nomsTables <- paste0("Modalite", 1:12)

# Maraichage
Modalite1 <- calculPartsDestination(PartComMaraic__1, `Autoconsommation`)
Modalite2 <- calculPartsDestination(PartComMaraic__2, `Alimentation des animaux`)
Modalite3 <- calculPartsDestination(PartComMaraic__3, `Dons (à la famille ou à des amis)`)
Modalite4 <- calculPartsDestination(PartComMaraic__4, `Echange`)
Modalite5 <- calculPartsDestination(PartComMaraic__5, `Vente directe au particulier`)
Modalite6 <- calculPartsDestination(PartComMaraic__6, `Vente par internet (Facebook ou autre site)`)
Modalite7 <- calculPartsDestination(PartComMaraic__7, `Vente à un commerçant ou artisan ou revendeur`)
Modalite8 <- calculPartsDestination(PartComMaraic__8, `Vente à un grossiste`)
Modalite9 <- calculPartsDestination(PartComMaraic__9, `Vente à un transformateur ou préparateur (y compris abattoir)`)
Modalite10 <- calculPartsDestination(PartComMaraic__10, `Vente à la coopérative ou au syndicat`)
Modalite11 <- calculPartsDestination(PartComMaraic__11, `Vente à la restauration collective`)
Modalite12 <- calculPartsDestination(PartComMaraic__12, `Vente aux restaurants (hors collectifs) / hôtels`)

listeTables <- lapply(nomsTables, get)
TCD24 <- reduce(listeTables, left_join, by = c("interview__key", "Archipel_1", "Ile", "Commune"))
writeCSV(TCD24)

# Vivrier
Modalite1 <- calculPartsDestination(PartComVivri__1, `Autoconsommation`)
Modalite2 <- calculPartsDestination(PartComVivri__2, `Alimentation des animaux`)
Modalite3 <- calculPartsDestination(PartComVivri__3, `Dons (à la famille ou à des amis)`)
Modalite4 <- calculPartsDestination(PartComVivri__4, `Echange`)
Modalite5 <- calculPartsDestination(PartComVivri__5, `Vente directe au particulier`)
Modalite6 <- calculPartsDestination(PartComVivri__6, `Vente par internet (Facebook ou autre site)`)
Modalite7 <- calculPartsDestination(PartComVivri__7, `Vente à un commerçant ou artisan ou revendeur`)
Modalite8 <- calculPartsDestination(PartComVivri__8, `Vente à un grossiste`)
Modalite9 <- calculPartsDestination(PartComVivri__9, `Vente à un transformateur ou préparateur (y compris abattoir)`)
Modalite10 <- calculPartsDestination(PartComVivri__10, `Vente à la coopérative ou au syndicat`)
Modalite11 <- calculPartsDestination(PartComVivri__11, `Vente à la restauration collective`)
Modalite12 <- calculPartsDestination(PartComVivri__12, `Vente aux restaurants (hors collectifs) / hôtels`)

listeTables <- lapply(nomsTables, get)
TCD25 <- reduce(listeTables, left_join, by = c("interview__key", "Archipel_1", "Ile", "Commune"))
writeCSV(TCD25)
