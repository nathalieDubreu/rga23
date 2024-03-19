rga23_surfacesCultures_horsCocoEtFourrag <- rga23_surfacesCultures |>
  filter(TypeCulture != 70 & culture_id != 307 & culture_id != 308 & culture_id != 309) |>
  mutate(TypeCulture = case_when(
    (TypeCulture == 10) ~ "10 - Cultures maraîchères",
    (TypeCulture == 20) ~ "20 - Cultures vivrières",
    (TypeCulture == 30) ~ "30 - Cultures fruitières (hors cocoteraies)",
    (TypeCulture == 40) ~ "40 - Feuillages et cultures florales (hors pépinières)",
    (TypeCulture == 50) ~ "50 - Plantes aromatiques, stimulantes et médicinales",
    (TypeCulture == 60) ~ "60 - Pépinières (plantes vendues en pot)",
    (TypeCulture == 70) ~ "70 - Cultures fourragères",
    (TypeCulture == 80) ~ "80 - Jachères",
    TRUE ~ as.character(TypeCulture)
  ))

## Tableaux pour la partie CULTURES

## Surfaces de cultures classiques par type et archipel

surfacesParTypeHorsCocoFourragEtArchipel <- left_join(
  rga23_surfacesCultures_horsCocoEtFourrag,
  rga23_champ |> select(interview__key, Archipel_1)
) |>
  group_by(Archipel_1, TypeCulture) |>
  summarize(
    `Nb Exploitants` = n_distinct(interview__key),
    `Surface (m2)` = sum(SurfaceCult, na.rm = TRUE),
    `Surface moyenne (m2)` = round(`Surface (m2)` / `Nb Exploitants`)
  )

surfacesParTypeHorsCocoFourrag <- rga23_surfacesCultures_horsCocoEtFourrag |>
  mutate(Archipel_1 = "Total") |>
  group_by(Archipel_1, TypeCulture) |>
  summarize(
    `Nb Exploitants` = n_distinct(interview__key),
    `Surface (m2)` = sum(SurfaceCult, na.rm = TRUE),
    `Surface moyenne (m2)` = round(`Surface (m2)` / `Nb Exploitants`)
  )

surfacesParTypeCultureArchipelEtTotal <- rbind(
  surfacesParTypeHorsCocoFourragEtArchipel,
  surfacesParTypeHorsCocoFourrag
) |>
  pivot_wider(names_from = c(Archipel_1), values_from = c(`Nb Exploitants`, `Surface (m2)`, `Surface moyenne (m2)`), values_fill = 0)

writeCSV(surfacesParTypeCultureArchipelEtTotal)


## Surfaces de cultures classiques par type, sexe de l'exploitant et archipel

surfacesParTypeHorsCocoFourragEtArchipelS <- left_join(
  rga23_surfacesCultures_horsCocoEtFourrag,
  rga23_champ |> select(interview__key, Archipel_1)
) |>
  left_join(rga23_mainOeuvre |> select(interview__key, SexeChefExpl)) |>
  group_by(Archipel_1, TypeCulture, SexeChefExpl) |>
  summarize(
    `Nb Exploitants` = n_distinct(interview__key),
    `Surface (m2)` = sum(SurfaceCult, na.rm = TRUE),
    `Surface moyenne (m2)` = round(`Surface (m2)` / `Nb Exploitants`)
  )

surfacesParTypeHorsCocoFourragS <- rga23_surfacesCultures_horsCocoEtFourrag |>
  left_join(rga23_mainOeuvre |> select(interview__key, SexeChefExpl)) |>
  mutate(Archipel_1 = "Total") |>
  group_by(Archipel_1, TypeCulture, SexeChefExpl) |>
  summarize(
    `Nb Exploitants` = n_distinct(interview__key),
    `Surface (m2)` = sum(SurfaceCult, na.rm = TRUE),
    `Surface moyenne (m2)` = round(`Surface (m2)` / `Nb Exploitants`)
  )

surfacesParTypeCultureArchipelEtTotalSexe <- rbind(
  surfacesParTypeHorsCocoFourragEtArchipelS,
  surfacesParTypeHorsCocoFourragS
) |>
  pivot_wider(names_from = c(Archipel_1, SexeChefExpl), values_from = c(`Nb Exploitants`, `Surface (m2)`, `Surface moyenne (m2)`), values_fill = 0)

writeCSV(surfacesParTypeCultureArchipelEtTotalSexe)

## Jardins océaniens

surfacesJOArchipel <- left_join(
  rga23_prodVegetales |> filter(ModesProduction__4 == 1),
  rga23_champ |> select(interview__key, Archipel_1)
) |>
  mutate(
    SurfaceBioJardins = case_when(
      SurfaceBioJardins == 1 ~ SurfaceJardins,
      TRUE ~ 0
    ),
    TypeCulture = "Jardins océaniens"
  ) |>
  group_by(Archipel_1, TypeCulture) |>
  summarize(
    `Nb Exploitants` = n_distinct(interview__key),
    `Surface (m2)` = sum(SurfaceJardins, na.rm = TRUE),
    `Surface moyenne (m2)` = round(`Surface (m2)` / `Nb Exploitants`),
  )

writeCSV(surfacesJOArchipel)

ensembleSurfacesTypeJoArchipel <- rbind(surfacesParTypeHorsCocoFourragEtArchipel, surfacesJOArchipel)

surfacesJOArchipelSexe <- left_join(
  rga23_prodVegetales |> filter(ModesProduction__4 == 1),
  rga23_champ |> select(interview__key, Archipel_1)
) |>
  left_join(rga23_mainOeuvre |> select(interview__key, SexeChefExpl)) |>
  mutate(
    SurfaceBioJardins = case_when(
      SurfaceBioJardins == 1 ~ SurfaceJardins,
      TRUE ~ 0
    ),
    TypeCulture = "Jardins océaniens"
  ) |>
  group_by(Archipel_1, TypeCulture, SexeChefExpl) |>
  summarize(
    `Nb Exploitants` = n_distinct(interview__key),
    `Surface (m2)` = sum(SurfaceJardins, na.rm = TRUE),
    `Surface moyenne (m2)` = round(`Surface (m2)` / `Nb Exploitants`),
  )

surfacesJOSexe <- left_join(
  rga23_prodVegetales |> filter(ModesProduction__4 == 1),
  rga23_mainOeuvre |> select(interview__key, SexeChefExpl)
) |>
  mutate(
    TypeCulture = "Jardins océaniens",
    Archipel_1 = "Total"
  ) |>
  group_by(Archipel_1, TypeCulture, SexeChefExpl) |>
  summarize(
    `Nb Exploitants` = n_distinct(interview__key),
    `Surface (m2)` = sum(SurfaceJardins, na.rm = TRUE),
    `Surface moyenne (m2)` = round(`Surface (m2)` / `Nb Exploitants`),
  )

surfacesJoArchipelEtTotalSexe <- rbind(
  surfacesJOArchipelSexe,
  surfacesJOSexe
) |>
  pivot_wider(names_from = c(Archipel_1, SexeChefExpl), values_from = c(`Nb Exploitants`, `Surface (m2)`, `Surface moyenne (m2)`), values_fill = 0)

writeCSV(surfacesJoArchipelEtTotalSexe)

### Tableau pour l'encadré

surfaceCultFourrageres <- rga23_surfacesCultures |>
  filter(TypeCulture == 70) |>
  mutate(TypeCulture = case_when(
    (culture_id == 701) ~ "70a - Cultures fourragères : pâturages",
    (culture_id == 702) ~ "70a - Cultures fourragères : pâturages",
    (culture_id == 703) ~ "70b - Cultures fourragères : maïs fourrage et ensilage et sorgho",
    (culture_id == 704) ~ "70b - Cultures fourragères : maïs fourrage et ensilage et sorgho",
    (culture_id == 705) ~ "70a - Cultures fourragères : pâturages",
    TRUE ~ as.character(TypeCulture)
  )) |>
  group_by(TypeCulture) |>
  summarize(
    `Surface (m2)` = sum(SurfaceCult, na.rm = TRUE)
  )

encadreSAUTypeTotal <- rbind(
  surfaceCultFourrageres,
  surfacesParTypeHorsCocoFourrag |> ungroup() |>
    select(-Archipel_1, -`Nb Exploitants`, -`Surface moyenne (m2)`),
  surfacesJO <- rga23_prodVegetales |>
    filter(ModesProduction__4 == 1) |>
    mutate(
      TypeCulture = "Jardins océaniens"
    ) |>
    group_by(TypeCulture) |>
    summarize(`Surface (m2)` = sum(SurfaceJardins, na.rm = TRUE))
) |>
  arrange(TypeCulture)
encadreSAUTypeTotalHa <- encadreSAUTypeTotal |>
  mutate(`Surface (Ha)` = round(`Surface (m2)` / 10000)) |>
  select(-`Surface (m2)`)
writeCSV(encadreSAUTypeTotalHa)

hectaresCulturesVegetales <- encadreSAUTypeTotal |>
  filter(TypeCulture != "70a - Cultures fourragères : pâturages") |>
  summarize(`Surface (Ha)` = round(sum(`Surface (m2)`) / 10000))

surfacePaturagesArchipel <- left_join(
  rga23_surfacesCultures,
  rga23_champ |> select(interview__key, Archipel_1)
) |>
  filter(culture_id == 701 | culture_id == 702 | culture_id == 705) |>
  group_by(Archipel_1) |>
  summarize(
    `Surface de pâturages (Ha)` = round(sum(SurfaceCult, na.rm = TRUE) / 10000)
  )
writeCSV(surfacePaturagesArchipel)


### Surfaces par archipel HORS paturages et hors cocoteraies

rbind(
  surfacesJOArchipel |>
    select(-`Surface moyenne (m2)`, `Nb Exploitants`) |>
    mutate(TypeCulture = 90),
  rga23_surfacesCultures_horsCocoEtPaturage <- left_join(
    rga23_surfacesCultures |>
      filter(culture_id != 701 & culture_id != 702 & culture_id != 705 & culture_id != 307 & culture_id != 308 & culture_id != 309),
    rga23_champ |> select(interview__key, Archipel_1)
  ) |>
    group_by(Archipel_1, TypeCulture) |>
    summarize(
      `Surface (m2)` = sum(SurfaceCult, na.rm = TRUE)
    )
) |>
  group_by(Archipel_1) |>
  summarize(
    `Surface (Ha)` = round(sum(`Surface (m2)`)/ 10000)
  )
