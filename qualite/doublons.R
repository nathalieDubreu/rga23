doublonsNonInterroges <- readCSV("rga23.csv") |>
  filter(statut_collecte == 5 & (interview__status == "120" | interview__status == "130")) |>
  select(interview__key, id_exploitation, assignment__id, id_enqueteur_ech) |>
  rename(idExploitDoublon = id_exploitation, numAffectationDoublon = assignment__id)

commentaires <- left_join(doublonsNonInterroges, readTable("interview__comments.tab", dossier)) |>
  select(!interview__id & !roster & !id1 & !id2)

# writeCSV(commentaires)

## A ajouter dans le fichier doublons_vX.csv et augmenter la version pour la lecture suivante
nouveauxDoublonsNonInterroges <- anti_join(doublonsNonInterroges, readCSV("doublons_v3.csv") |>
                                             select(interview__keyDoublon) |>
                                             rename(interview__key = interview__keyDoublon))

## A supprimer si besoin et augmenter la version pour la lecture suivante
anciensDoublonsNonInterroges <- anti_join(readCSV("doublons_v3.csv") |> 
                                             select(interview__keyDoublon) |>
                                             rename(interview__key = interview__keyDoublon),doublonsNonInterroges)

# writeCSV(nouveauxDoublonsNonInterroges)

## Et ceux qui n'existent plus...
# existePlus <- readCSV("rga23.csv") |>
#   filter(statut_collecte == 2 & (interview__status == "120" | interview__status == "130")) |>
#   select(interview__key, id_exploitation, assignment__id) |>
#   rename(idExploitDoublon = id_exploitation, numAffectationDoublon = assignment__id)
#
# commentairesExistePlus <- left_join(existePlus, readTable("interview__comments.tab", dossier)) |>
#   select(!interview__id & !roster & !id1 & !id2)

doublonsManquants <- readCSV("doublons_v3.csv") |>
  filter(is.na(idExploit) & is.na(numAffectation)) |>
  select(interview__keyDoublon, idExploitDoublon, numAffectationDoublon, comment, TODO) |>
  rename(interview__key = interview__keyDoublon)

nouveauxCommentaires <- left_join(doublonsManquants, readTable("interview__comments.tab", dossier), by = c("interview__key")) |>
  select(!interview__id & !roster & !id1 & !id2)

writeCSV(nouveauxCommentaires)
