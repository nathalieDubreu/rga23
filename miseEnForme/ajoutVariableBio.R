nonBio <- readInputCSV("nonBio.csv") |>
  mutate(nonBioDag = 1)

rga23 <- left_join(rga23, nonBio, by = "interview__key") |>
  mutate(AgriBio_DAG = case_when(
    nonBioDag == 1 ~ "2",
    TRUE ~ AgriBio
  ))

writeCSVTraites(rga23)
