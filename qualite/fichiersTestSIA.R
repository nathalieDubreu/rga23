rga23 <- readCSV("rga23.csv")

rga23Filtre <- rga23 |> filter((((eligibiliteCoprah == 1 & substring(id_exploitation, 0, 2) == "C1" & nchar(id_exploitation) == 5 & !grepl("5", id_exploitation) & !grepl("6", id_exploitation)) |
  (eligibilite == 1 & ((substring(id_exploitation, 0, 1) == "P" & nchar(id_exploitation) == 5 & !grepl("5", id_exploitation)) | (substring(id_exploitation, 0, 1) == "M" & !grepl("5", id_exploitation) & grepl("2", id_exploitation)))) |
  ((eligibilite == 1 | eligibiliteCoprah == 1) & substring(id_exploitation, 0, 1) == "X")) & grepl("1", id_exploitation) & grepl("9", id_exploitation)) | ((eligibiliteCoprah == 0 & substring(id_exploitation, 0, 2) == "C1" & nchar(id_exploitation) == 5 & !grepl("5", id_exploitation) & !grepl("6", id_exploitation)) |
  (eligibilite == 0 & grepl("9", id_exploitation) & ((substring(id_exploitation, 0, 1) == "P" & nchar(id_exploitation) == 5 & grepl("5", id_exploitation)) |
    (substring(id_exploitation, 0, 1) == "M" & !grepl("5", id_exploitation) & grepl("2", id_exploitation)))) |
  ((eligibilite == 0 & eligibiliteCoprah == 0) & substring(id_exploitation, 0, 1) == "X")) & grepl("1", id_exploitation))

rga23 |>
  group_by(substring(id_exploitation, 0, 1)) |>
  count()
rga23Filtre |>
  group_by(substring(id_exploitation, 0, 1)) |>
  count()

inner_join(rga23Filtre, idExploitantsDansLeChamp) |>
  group_by(substring(id_exploitation, 0, 1)) |>
  count()

idRGAFiltre <- rga23Filtre |> select(interview__key)

selectionTest <- function(table) {
  table <- inner_join(table, idRGAFiltre)
  return(table)
}

rga23_gestion <- selectionTest(rga23_gestion)
rga23_peche <- selectionTest(rga23_peche)
rga23_coprahculteurs <- selectionTest(rga23_coprahculteurs)
rga23_exploitations <- selectionTest(rga23_exploitations)
rga23_general <- selectionTest(rga23_general)
rga23_mainOeuvre <- selectionTest(rga23_mainOeuvre)
rga23_prodAnimales <- selectionTest(rga23_prodAnimales)
rga23_prodVegetales <- selectionTest(rga23_prodVegetales)
rga23_tape <- selectionTest(rga23_tape)

writeCSVTraites(rga23_gestion, file.path(Sys.getenv("cheminAcces"), "TestSIA"))
writeCSVTraites(rga23_peche, file.path(Sys.getenv("cheminAcces"), "TestSIA"))
writeCSVTraites(rga23_coprahculteurs, file.path(Sys.getenv("cheminAcces"), "TestSIA"))
writeCSVTraites(rga23_exploitations, file.path(Sys.getenv("cheminAcces"), "TestSIA"))
writeCSVTraites(rga23_general, file.path(Sys.getenv("cheminAcces"), "TestSIA"))
writeCSVTraites(rga23_mainOeuvre, file.path(Sys.getenv("cheminAcces"), "TestSIA"))
writeCSVTraites(rga23_prodAnimales, file.path(Sys.getenv("cheminAcces"), "TestSIA"))
writeCSVTraites(rga23_prodVegetales, file.path(Sys.getenv("cheminAcces"), "TestSIA"))
writeCSVTraites(rga23_tape, file.path(Sys.getenv("cheminAcces"), "TestSIA"))

rga23_cocoteraies <- selectionTest(rga23_cocoteraies)
rga23_coexploitants <- selectionTest(rga23_coexploitants)
rga23_engraisOrganiques <- selectionTest(rga23_engraisOrganiques)
rga23_moPermanenteFam <- selectionTest(rga23_moPermanenteFam)
rga23_parcelles <- selectionTest(rga23_parcelles)
rga23_sites <- selectionTest(rga23_sites)
rga23_surfacesCultures <- selectionTest(rga23_surfacesCultures)

writeCSVTraites(rga23_cocoteraies, file.path(Sys.getenv("cheminAcces"), "TestSIA"))
writeCSVTraites(rga23_coexploitants, file.path(Sys.getenv("cheminAcces"), "TestSIA"))
writeCSVTraites(rga23_engraisOrganiques, file.path(Sys.getenv("cheminAcces"), "TestSIA"))
writeCSVTraites(rga23_moPermanenteFam, file.path(Sys.getenv("cheminAcces"), "TestSIA"))
writeCSVTraites(rga23_parcelles, file.path(Sys.getenv("cheminAcces"), "TestSIA"))
writeCSVTraites(rga23_sites, file.path(Sys.getenv("cheminAcces"), "TestSIA"))
writeCSVTraites(rga23_surfacesCultures, file.path(Sys.getenv("cheminAcces"), "TestSIA"))
