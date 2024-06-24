# Demande DRM - récupération des données relatives aux pêcheurs (pas de filtre sur des seuils)

rga23_pecheurs <- readCSV("rga23_tousFichiersPlats.csv") |>
  filter(ActivitesChefExploit__5 == 1)
# 1963 chefs d'exploitations ou coprahculteurs déclarent une activité de pêche

rga23_pecheurs_engraisOrganiques <- left_join(readCSV("rga23_engraisOrganiques.csv"),
  rga23_pecheurs |> select(interview__key),
  by = "interview__key"
)


writeCSV(rga23_pecheurs)
writeCSV(rga23_pecheurs_engraisOrganiques)