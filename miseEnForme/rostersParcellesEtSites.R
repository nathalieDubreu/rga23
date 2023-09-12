rga23_sites <- readTable("roster_accesSite.tab", dossier) |> select(!interview__id)
rga23_parcelles <- readTable("roster_parcelles.tab", dossier) |> select(!interview__id)

writeCSVTraites(rga23_sites)
writeCSVTraites(rga23_parcelles)
