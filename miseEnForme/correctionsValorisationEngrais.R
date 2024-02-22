engraisUtilise <- rga23_engraisOrganiques |>
  group_by(interview__key) |>
  summarize(
    nbEngraisValorises = sum(DonEngraisOrga == 1 | VenteEngraisOrga == 1 | EpandageEngraisOrga == 1, na.rm = TRUE),
    aucuneValorisation = ifelse(nbEngraisValorises == 0,1,0)
  )

left_join(rga23, engraisUtilise) |>
  filter(aucuneValorisation == 1 & PropRecyclEngraisOrga != 1) |>
  group_by(PropRecyclEngraisOrga) |>
  count()
# 2    74
# 3    25
# 4    26
