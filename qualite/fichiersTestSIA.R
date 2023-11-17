source("champs/champRGA.R")

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
  table <- inner_join(table, idRGAFiltre, by = c("interview__key"))
  return(table)
}

# rga23_gestion <- selectionTest(readCSV("rga23_gestion.csv"))
# rga23_peche <- selectionTest(readCSV("rga23_peche.csv"))
rga23_coprahculteurs <- selectionTest(readCSV("rga23_coprahculteurs.csv"))
rga23_exploitations <- selectionTest(readCSV("rga23_exploitations.csv"))
rga23_general <- selectionTest(readCSV("rga23_general.csv")) |> select(
  !SituationConjChefExpl &
    !ActiviteConjoint__1 &
    !ActiviteConjoint__2 &
    !ActiviteConjoint__3 &
    !ActiviteConjoint__4 &
    !ActiviteConjoint__5 &
    !ActiviteConjoint__6 &
    !ActiviteConjoint__7 &
    !ActiviteConjoint__8 &
    !ActiviteConjoint__9 &
    !ActiviteConjoint__10 &
    !ActiviteConjoint__11 &
    !ActiviteConjoint__12
)
rga23_mainOeuvre <- selectionTest(readCSV("rga23_mainOeuvre.csv"))
rga23_prodAnimales <- selectionTest(readCSV("rga23_prodAnimales.csv"))
rga23_prodVegetales <- selectionTest(readCSV("rga23_prodVegetales.csv"))
rga23_tape <- selectionTest(readCSV("rga23_tape.csv")) |> select(
  !PropTravailPenibleExpl &
    !AvenirAgri &
    !MoyensCompetences &
    !BesoinsSatisf &
    !Economies &
    !FreqEvenComLocale &
    !ProbActivite__1 &
    !ProbActivite__2 &
    !ProbActivite__3 &
    !ProbActivite__4 &
    !NoteSatisfactionExpl &
    !ConnaissPratiques &
    !InteretPratiques &
    !ConnaissOrga &
    !FruitsLocaux &
    !PoissonsLocaux &
    !LegumesLocaux &
    !VivriLocaux &
    !PlatsTradi &
    !MaaTahitiPlus &
    !BesoinsNutri
)
# writeCSVTraites(rga23_gestion, file.path(Sys.getenv("cheminAcces"), "TestSIA"))
# writeCSVTraites(rga23_peche, file.path(Sys.getenv("cheminAcces"), "TestSIA"))
writeCSVTraites(rga23_coprahculteurs, file.path(Sys.getenv("cheminAcces"), "TestSIA"))
writeCSVTraites(rga23_exploitations, file.path(Sys.getenv("cheminAcces"), "TestSIA"))
writeCSVTraites(rga23_general, file.path(Sys.getenv("cheminAcces"), "TestSIA"))
writeCSVTraites(rga23_mainOeuvre, file.path(Sys.getenv("cheminAcces"), "TestSIA"))
writeCSVTraites(rga23_prodAnimales, file.path(Sys.getenv("cheminAcces"), "TestSIA"))
writeCSVTraites(rga23_prodVegetales, file.path(Sys.getenv("cheminAcces"), "TestSIA"))
writeCSVTraites(rga23_tape, file.path(Sys.getenv("cheminAcces"), "TestSIA"))

rga23_cocoteraies <- selectionTest(readCSV("rga23_cocoteraies.csv"))
rga23_coexploitants <- selectionTest(readCSV("rga23_coexploitants.csv"))
rga23_engraisOrganiques <- selectionTest(readCSV("rga23_engraisOrganiques.csv"))
rga23_moPermanenteFam <- selectionTest(readCSV("rga23_moPermanenteFam.csv"))
rga23_parcelles <- selectionTest(readCSV("rga23_parcelles.csv"))
rga23_sites <- selectionTest(readCSV("rga23_sites.csv"))
rga23_surfacesCultures <- selectionTest(readCSV("rga23_surfacesCultures.csv"))

writeCSVTraites(rga23_cocoteraies, file.path(Sys.getenv("cheminAcces"), "TestSIA"))
writeCSVTraites(rga23_coexploitants, file.path(Sys.getenv("cheminAcces"), "TestSIA"))
writeCSVTraites(rga23_engraisOrganiques, file.path(Sys.getenv("cheminAcces"), "TestSIA"))
writeCSVTraites(rga23_moPermanenteFam, file.path(Sys.getenv("cheminAcces"), "TestSIA"))
writeCSVTraites(rga23_parcelles, file.path(Sys.getenv("cheminAcces"), "TestSIA"))
writeCSVTraites(rga23_sites, file.path(Sys.getenv("cheminAcces"), "TestSIA"))
writeCSVTraites(rga23_surfacesCultures, file.path(Sys.getenv("cheminAcces"), "TestSIA"))

# baseRGA23 <- readCSV("BaseRGA_v9.csv")
# tablePassageIdentifiants <- inner_join(rga23_general |> select(interview__key, id_exploitation), baseRGA23)
# writeCSV(tablePassageIdentifiants)
