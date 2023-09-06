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
  )) |>
  mutate(PartRevenusAgriExpl = case_when(
    (ActivitesChefExploit__1 == 1 & is.na(PartRevenusAgriExpl)) ~ 4,
    TRUE ~ PartRevenusAgriExpl
  )) |>
  mutate(PresSurfIrrigables = case_when(
    (Irrigation == 1) ~ 1,
    TRUE ~ PresSurfIrrigables
  )) |>
  mutate(SurfaceBioJardins = case_when(
    (AgriBio == 1 & !is.na(SurfaceJardins)) ~ 1,
    (AgriBio == 2 & !is.na(SurfaceJardins)) ~ 2,
    TRUE ~ SurfaceBioJardins
  )) |>
  mutate(SurfaceIrrigJardins = case_when(
    (Irrigation == 2 & !is.na(SurfaceJardins)) ~ 0,
    TRUE ~ SurfaceIrrigJardins
  ))

rga23 <- rga23Brut
