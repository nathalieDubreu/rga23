doublonsNonInterroges <- readCSV("rga23.csv") |>
  filter(statut_collecte == 5 & (interview__status == "120" | interview__status == "130")) |>
  select(interview__key, id_exploitation, assignment__id) |>
  rename(idExploitDoublon = id_exploitation, numAffectationDoublon = assignment__id)

commentaires <- left_join(doublonsNonInterroges, readTable("interview__comments.tab", dossier)) |>
  select(!interview__id & !roster & !id1 & !id2)


## Et ceux qui n'existent plus...
# existePlus <- readCSV("rga23.csv") |>
#   filter(statut_collecte == 2 & (interview__status == "120" | interview__status == "130")) |>
#   select(interview__key, id_exploitation, assignment__id) |>
#   rename(idExploitDoublon = id_exploitation, numAffectationDoublon = assignment__id)
# 
# commentairesExistePlus <- left_join(existePlus, readTable("interview__comments.tab", dossier)) |>
#   select(!interview__id & !roster & !id1 & !id2)


