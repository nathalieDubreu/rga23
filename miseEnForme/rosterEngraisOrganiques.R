rga23_engraisOrganiques <- readTable("roster_engrais_orga.tab", dossier) |>
  filter(interview__key != "59-36-31-34" &
    interview__key != "06-79-34-97" &
    interview__key != "26-72-53-00" &
    interview__key != "49-29-35-86" &
    interview__key != "93-83-94-94") |>
  select(!interview__id & !epandDejection) |>
  rename(engraisOrga_id = roster_engrais_orga__id)

writeCSVTraites(rga23_engraisOrganiques)
