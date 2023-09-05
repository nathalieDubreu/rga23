exportRGA <- readTable("rga23.tab", dossier)

eligiblesRGA <- exportRGA |>
  filter((interview__status == 100 | interview__status == 120) & statut_collecte == 1) |>
  filter((eligibiliteCoprah == 1 & substring(id_exploitation, 0, 1) == "C") |
    (eligibilite == 1 & (substring(id_exploitation, 0, 1) == "P" | substring(id_exploitation, 0, 1) == "M")) |
    ((eligibilite == 1 | eligibiliteCoprah == 1) & substring(id_exploitation, 0, 1) == "X"))

##eligiblesRGA |> filter(ActivitesChefExploit__5 == 1) |> group_by(IleExploitation, id_enqueteur_ech) |> count()

eligiblesRGA |> filter(substring(id_exploitation, 0, 1) == "X" & eligibilite == FALSE & eligibiliteCoprah == TRUE) |> count()
