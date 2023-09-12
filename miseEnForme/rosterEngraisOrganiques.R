rga23_engraisOrganiques <- readTable("roster_engrais_orga.tab", dossier) |> select(!interview__id)

writeCSVTraites(rga23_engraisOrganiques)
