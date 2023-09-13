rga23_sites <- readTable("roster_accesSite.tab", dossier) |> select(!interview__id) |>
  rename(site_id = roster_accesSite__id)
rga23_parcelles <- readTable("roster_parcelles.tab", dossier) |> select(!interview__id)|>
  rename(parcelle_id = roster_parcelles__id, site_id = roster_accesSite__id)

writeCSVTraites(rga23_sites)
writeCSVTraites(rga23_parcelles)
