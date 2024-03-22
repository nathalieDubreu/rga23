# Définir les filtres avant de lancer les fonctions
## La mise en place d'un filtre nécessite simplement la liste des interview__key et le préfixe aux noms des fichiers

## Filtre 1 - Uniquement les exploitations de RAPA
# interviewKeysAConserver <- readCSV("rga23_exploitations.csv") |>
#   filter(IleExploitation == "Rapa") |>
#   distinct(interview__key)
# prefixeFichiers <- "Rapa"

## Filtre 2 - Uniquement les exploitations qui atteignent ou dépassent les 400 points CAPL (y compris les producteurs de coprah éligibles de plus de 2,7 tonnes)
source("champs/champCAPL.R")
interviewKeysAConserver <- idExploitantsPointsCAPL |>
  filter(indicRGA23_Coprah == 1 | PointsCAPL >= 400) |>
  select(interview__key)
prefixeFichiers <- "CAPL"

## Filtre 3 - Uniquement les exploitations qui respectent au moins un des seuils du RGA 2023
interviewKeysAConserver <- readCSV("rga23_general.csv") |>
  filter(indicRGA23 == 1) |>
  select(interview__key)
prefixeFichiers <- "Valides2023"

## Filtre 4 - Uniquement les exploitations qui respectent au moins un des seuils de 2012
interviewKeysAConserver <- readCSV("rga23_general.csv") |>
  filter(indicRGA12 == 1) |>
  select(interview__key)
prefixeFichiers <- "Valides2012"


# A LANCER une fois le filtre préparé

filtrerDonnees <- function(table) {
  donneesFiltree <- inner_join(readCSV({{table}}), interviewKeysAConserver, by="interview__key")
  chemin <- file.path(Sys.getenv("cheminAcces"), paste0(prefixeFichiers,"_", table))
  readr::write_csv2(donneesFiltree, file = chemin)
}

filtrerDonnees("rga23_general.csv")
filtrerDonnees("rga23_exploitations.csv")
filtrerDonnees("rga23_coprahculteurs.csv")
filtrerDonnees("rga23_mainOeuvre.csv")
filtrerDonnees("rga23_tape.csv")
filtrerDonnees("rga23_prodVegetales.csv")
filtrerDonnees("rga23_prodAnimales.csv")
filtrerDonnees("rga23_peche.csv")
filtrerDonnees("rga23_cocoteraies.csv")
filtrerDonnees("rga23_coexploitants.csv")
filtrerDonnees("rga23_engraisOrganiques.csv")
filtrerDonnees("rga23_moPermanenteFam.csv")
filtrerDonnees("rga23_parcelles.csv")
filtrerDonnees("rga23_sites.csv")
filtrerDonnees("rga23_surfacesCultures.csv")


