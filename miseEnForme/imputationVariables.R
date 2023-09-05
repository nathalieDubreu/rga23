rga23Brut <- readTable("rga23.tab", dossier) |>
  mutate(AbeillesBio = case_when(
    (AgriBio == 1 & PresenceAnimaux__7 == 1) ~ 1,
    (AgriBio == 2 & PresenceAnimaux__7 == 1) ~ 2,
    TRUE ~ AbeillesBio
  )) |>
  mutate(PartPlantsAutoP = case_when(
    (ProvenancePlants__2 == 0) ~ 1,
    (ProvenancePlants__2 == 1 & is.na(PartPlantsAutoP)) ~ 5,
    TRUE ~ PartPlantsAutoP
  )) |>
  mutate(PartSemencesAutoP = case_when(
    (ProvenanceSemences__2 == 0) ~ 1,
    (ProvenanceSemences__2 == 1 & is.na(PartSemencesAutoP)) ~ 5,
    TRUE ~ PartSemencesAutoP
  ))|>
  mutate(PartRevenusAgriExpl = case_when(
    (ActivitesChefExploit__1 == 1 & is.na(PartRevenusAgriExpl)) ~ 4,
    TRUE ~ PartRevenusAgriExpl
  ))

rga23 <- rga23Brut
  

rga23Brut |> group_by(PartRevenusAgriExpl) |> count()
rga23Brut |> group_by(PartRevenusAgriExpl2) |> count()
