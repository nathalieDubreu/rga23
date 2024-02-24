engraisUtilise <- rga23_engraisOrganiques |>
  group_by(interview__key) |>
  summarize(
    nbEngraisValorises = sum(DonEngraisOrga == 1 | VenteEngraisOrga == 1 | EpandageEngraisOrga == 1, na.rm = TRUE),
    aucuneValorisation = ifelse(nbEngraisValorises == 0,1,0)
  )

rga23B <- left_join(rga23, engraisUtilise) |>
  mutate(PropRecyclEngraisOrga = case_when(
    aucuneValorisation == 1 ~ "1",
    TRUE ~ PropRecyclEngraisOrga
  ))

