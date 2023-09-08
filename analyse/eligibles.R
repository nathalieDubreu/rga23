eligiblesRGA <- rga23 |>
  filter((interview__status == 100 | interview__status == 120) & statut_collecte == 1) |>
  filter((eligibiliteCoprah == 1 & substring(id_exploitation, 0, 1) == "C") |
    (eligibilite == 1 & (substring(id_exploitation, 0, 1) == "P" | substring(id_exploitation, 0, 1) == "M")) |
    ((eligibilite == 1 | eligibiliteCoprah == 1) & substring(id_exploitation, 0, 1) == "X"))

## eligiblesRGA |> filter(ActivitesChefExploit__5 == 1) |> group_by(IleExploitation, id_enqueteur_ech) |> count()

eligiblesRGA |>
  filter(substring(id_exploitation, 0, 1) == "X" &
    eligibilite == FALSE & eligibiliteCoprah == TRUE) |>
  count()

eligiblesRGA |>
  filter(substring(id_exploitation, 0, 1) == "X" &
           eligibilite == FALSE & eligibiliteCoprah == TRUE &
           interview__key != "05-81-20-62" &
           interview__key != "91-37-53-21" &
           interview__key != "38-13-87-05" &
           interview__key != "87-50-31-74" &
           interview__key != "88-51-69-94" &
           interview__key != "26-04-00-30") 

