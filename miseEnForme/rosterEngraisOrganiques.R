rga23_engraisOrganiques <- readTable("roster_engrais_orga.tab", dossier) |> select(!interview__id & !epandDejection) |>
  rename(engraisOrga_id = roster_engrais_orga__id)

writeCSVTraites(rga23_engraisOrganiques)
