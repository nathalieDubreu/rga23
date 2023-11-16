rga23 <- readCSV("rga23.csv")

eligiblesRGA <- rga23 |>
  filter((interview__status == 100 | interview__status == 120 | interview__status == 130) & statut_collecte == 1) |>
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
    interview__key != "26-04-00-30" &
    interview__key != "39-17-18-65" &
    interview__key != "92-89-85-76" &
    interview__key != "80-90-88-92" &
    interview__key != "74-71-45-49" &
    interview__key != "85-03-43-60" &
    interview__key != "15-49-55-56" &
    interview__key != "80-20-52-20" &
    interview__key != "42-24-21-22" &
    interview__key != "29-46-58-12" &
    interview__key != "70-24-84-72" &
    interview__key != "51-37-96-71" &
    interview__key != "15-34-36-43" &
    interview__key != "46-51-80-33" &
    interview__key != "73-82-87-44" &
    interview__key != "13-18-12-26" &
    interview__key != "92-27-93-63" &
    interview__key != "37-25-09-12" &
    interview__key != "68-62-34-63")

# test <- eligiblesRGA |> filter(interview__key=="25-94-37-62")

aVerifier <- eligiblesRGA |>
  filter(MOPermanenteFamiliale == 1 & NbMOPermFamiliale == 0) |>
  select(interview__key, interview__status, id_enqueteur_ech)
