rga23_sites <- readTable("roster_accesSite.tab", dossier) |>
  filter(!(interview__key %in% interviewKeyAExclure)) |>
  select(-interview__id, -erreurDomicile) |>
  mutate(
    sommeSurfacesSite = as.numeric(as.character(round(sommeSurfacesSite, 0))),
    sommeSurfacesMesureesSite = as.numeric(as.character(round(sommeSurfacesMesureesSite, 0)))
  ) |>
  rename(site_id = roster_accesSite__id)

rga23_parcelles <- readTable("roster_parcelles.tab", dossier) |>
  filter(!(interview__key %in% interviewKeyAExclure)) |>
  select(!interview__id) |>
  mutate(polygone__area = case_when(
    polygone__area == -999999999 ~ as.numeric(NA),
    TRUE ~ as.numeric(as.character(round(polygone__area, 0)))
  )) |>
  rename(parcelle_id = roster_parcelles__id, site_id = roster_accesSite__id)

writeCSVTraites(rga23_sites)
writeCSVTraites(rga23_parcelles)
