rga23_coexploitants <- readTable("RosterCoExploit.tab", dossier) |> select(!interview__id)
rga23_moPermanenteNonFam <- readTable("MONonFamPerm.tab", dossier) |> select(!interview__id)
rga23_moPermanenteFam <- readTable("RosterMOPermFam.tab", dossier) |> select(!interview__id)

writeCSVTraites(rga23_coexploitants)
writeCSVTraites(rga23_moPermanenteNonFam)
writeCSVTraites(rga23_moPermanenteFam)
