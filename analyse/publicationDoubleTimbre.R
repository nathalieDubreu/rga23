library(tidyr)

## Restriction au champ 23 du RGA
rga23_champ <- readCSV("rga23_gestion.csv") |>
  filter(indicRGA23 == 1) 

# Tables utiles - restreintes au champ
rga23_parcelles <- inner_join(readCSV("rga23_parcelles.csv"), rga23_champ |> select(interview__key))
rga23_prodVegetales <- inner_join(readCSV("rga23_prodVegetales.csv"), rga23_champ |> select(interview__key))
rga23_prodAnimales <- inner_join(readCSV("rga23_prodAnimales.csv"), rga23_champ |> select(interview__key, Archipel_1))
rga23_surfacesCultures <- inner_join(readCSV("rga23_surfacesCultures.csv"), rga23_champ |> select(interview__key))
rga23_tape <- inner_join(readCSV("rga23_tape.csv"), rga23_champ |> select(interview__key, Archipel_1))
rga23_coprahculteurs <- inner_join(readCSV("rga23_coprahculteurs.csv"), rga23_champ |> select(interview__key, indicRGA23_Coprah))
rga23_cocoteraies <- inner_join(readCSV("rga23_cocoteraies.csv"), rga23_champ |> select(interview__key))
rga23_exploitations <- inner_join(readCSV("rga23_exploitations.csv"), rga23_champ |> select(interview__key, RaisonsRecensement__1, RaisonsRecensement__2))
rga23_general <- inner_join(readCSV("rga23_general.csv"), rga23_champ |> select(interview__key, Archipel_1)) |>
  mutate(age = 2023 - as.numeric(substring(DateNaissChefExpl, 7, 10)))
rga23_mainOeuvre <- inner_join(readCSV("rga23_mainOeuvre.csv"), rga23_champ |>
  select(interview__key, Archipel_1)) |>
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

