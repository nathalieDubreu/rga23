rga23_coexploitants <- readTable("RosterCoExploit.tab", dossier) |> select(!interview__id)|>
  rename(coexploitant_id = RosterCoExploit__id)
writeCSVTraites(rga23_coexploitants)

rga23_moPermanenteFam <- readTable("RosterMOPermFam.tab", dossier) |> select(!interview__id) |>
  rename(persMoPermFam_id = RosterMOPermFam__id)
# Données récupérées par téléphone pour certains X dans la table rga23_moPermanenteFam
donneesMOPermFam <- readInputCSV("donneesMainOeuvrePermFam.csv")

# Regroupement des données
rga23_moPermanenteFam <- rbind(rga23_moPermanenteFam, donneesMOPermFam)

writeCSVTraites(rga23_moPermanenteFam)

rosterMoPermanenteNonFam <- readTable("MONonFamPerm.tab", dossier) |> select(!interview__id)

head(rosterMoPermanenteNonFam)

for (i in 1:4) {
  nBFemmesI <- paste("nbFemMONonFamPerm__", i, sep = "")
  nBHommesI <- paste("nbHomMONonFamPerm__", i, sep = "")

  TableI <-
    rosterMoPermanenteNonFam |> filter(MONonFamPerm__id == i)

  rga23_mainOeuvre <-
    left_join(
      rga23_mainOeuvre,
      TableI |> select(!MONonFamPerm__id),
      by = c("interview__key")
    ) |>
    rename(
      "{nBFemmesI}" := MOFemEmpl,
      "{nBHommesI}" := MOHomEmpl
    )
}

writeCSVTraites(rga23_mainOeuvre)

rm(
  TableI,
  rosterMoPermanenteNonFam)
