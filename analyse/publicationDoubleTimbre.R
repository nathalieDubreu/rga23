library(rmarkdown)
library(knitr)
library(tidyr)

source("champs/champRGA.R")

## Ajout d'une indicatrice dans la table RGA pour les exploitants dans le champ (cultures et/ou élevage)
rga23A <- left_join(rga23, idExploitantsDansLeChamp |> mutate(ValideRGA = 1),
  by = c("interview__key")
)

## Ajout d'une indicatrice dans la table RGA pour les coprahculteurs de plus de 2,7 tonnes (identifiants C et X éligibles)
rga23A_valides <- rga23A |>
  mutate(CoprahValideRGA = case_when((eligibiliteCoprah == 1 & substring(id_exploitation, 0, 1) == "C") | (eligibiliteCoprah == 1 & substring(id_exploitation, 0, 1) == "X") ~ 1))

## Restriction au champ 23 du RGA
rga23_champ <- rga23A_valides |>
  filter(ValideRGA == 1 | CoprahValideRGA == 1) |>
  mutate(Archipel_1 = case_when(!is.na(ArchipelExploitation) ~ ArchipelExploitation, TRUE ~ Archipel))

# Tables utiles - restreintes au champ
rga23_parcelles <- inner_join(readCSV("rga23_parcelles.csv"), rga23_champ |> select(interview__key))
rga23_prodVegetales <- inner_join(readCSV("rga23_prodVegetales.csv"), rga23_champ |> select(interview__key))
rga23_prodAnimales <- inner_join(readCSV("rga23_prodAnimales.csv"), rga23_champ |> select(interview__key))
rga23_surfacesCultures <- inner_join(readCSV("rga23_surfacesCultures.csv"), rga23_champ |> select(interview__key))
rga23_coprahculteurs <- inner_join(readCSV("rga23_coprahculteurs.csv"), rga23_champ |> select(interview__key, CoprahValideRGA))
rga23_exploitations <- inner_join(readCSV("rga23_exploitations.csv"), rga23_champ |> select(interview__key, RaisonsRecensement__1, RaisonsRecensement__2))
rga23_general <- inner_join(readCSV("rga23_general.csv"), rga23_champ |> select(interview__key)) |>
  mutate(age = 2023 - as.numeric(substring(DateNaissChefExpl, 7, 10)))
rga23_mainOeuvre <- inner_join(readCSV("rga23_mainOeuvre.csv"), rga23_champ |>
  select(interview__key)) |>
  mutate(totalMAOccas = ifelse(is.na(NbFemOccasAvecLien), 0, NbFemOccasAvecLien) +
    ifelse(is.na(NbFemOccasSansLien), 0, NbFemOccasSansLien) +
    ifelse(is.na(NbHomOccasAvecLien), 0, NbHomOccasAvecLien) +
    ifelse(is.na(NbHomOccasSansLien), 0, NbHomOccasSansLien))


source("analyse/exploitations.R")
source("analyse/prodVegetalesEtSurfaces.R")
source("analyse/prodAnimales.R")
source("analyse/chefExplEtMainOeuvre.R")
source("analyse/coprahculteurs.R")

rmarkdown::render("analyse/publicationDoubleTimbre.Rmd", encoding = "UTF-8")

rm(coExploitants, mainOeuvre, moPermFamiliale, poules, rga23_champ_date, rga23A, rga23A_valides, surfacesCultures, surfacesCulturesEligibles, table)
