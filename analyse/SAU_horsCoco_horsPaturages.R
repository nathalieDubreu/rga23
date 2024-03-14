rga23_surfacesCultures_horsCocoEtFourrag <- rga23_surfacesCultures |>
  filter(TypeCulture != 80 & culture_id != 307 & culture_id != 308 & culture_id != 309)

surfacesParTypeHorsCocoFourragEtArchipel <- left_join(rga23_surfacesCultures_horsCocoEtFourrag, rga23_champ |> select(interview__key, Archipel_1)) |>
  left_join(rga23_mainOeuvre |> select(interview__key, SexeChefExpl)) |>
  group_by(Archipel_1, TypeCulture, SexeChefExpl) |>
  summarize(
    `Nb Exploitants` = n_distinct(interview__key),
    `Surface (m2)` = sum(SurfaceCult, na.rm = TRUE),
    `Surface moyenne (m2)` = round(`Surface (m2)`/`Nb Exploitants`)
  )

surfacesParTypeHorsCocoFourrag <- rga23_surfacesCultures_horsCocoEtFourrag |>
  left_join(rga23_mainOeuvre |> select(interview__key, SexeChefExpl)) |>
  mutate(Archipel_1 = "Total") |>
  group_by(Archipel_1, TypeCulture, SexeChefExpl) |>
  summarize(
    `Nb Exploitants` = n_distinct(interview__key),
    `Surface (m2)` = sum(SurfaceCult, na.rm = TRUE), 
    `Surface moyenne (m2)` = round(`Surface (m2)`/`Nb Exploitants`)
  )

surfacesParTypeCultureArchipelEtTotalSexe <- rbind(surfacesParTypeHorsCocoFourragEtArchipel, surfacesParTypeHorsCocoFourrag) |>
  pivot_wider(names_from = c(Archipel_1, SexeChefExpl), values_from = c(`Nb Exploitants`, `Surface (m2)`, `Surface moyenne (m2)`), values_fill = 0)

writeCSV(surfacesParTypeCultureArchipelEtTotalSexe)
