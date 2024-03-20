interviewKeysAConserver <- readCSV("rga23_exploitations.csv") |>
  filter(IleExploitation == "Rapa") |>
  distinct(interview__key)

filtrerDonnees <- function(table) {
  donneesFiltree <- inner_join(readCSV({{table}}), interviewKeysAConserver, by="interview__key")
  chemin <- file.path(Sys.getenv("cheminAcces"), paste0("Rapa_", table))
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


