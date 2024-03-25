## Tableaux pour la partie CULTURES

## Surfaces de cultures classiques par type et archipel - HORS COCOTERAIES ET HORS CULTURES FOURRAGERES

surfacesParType_HC_HF_Archipel <- left_join(
  rga23_surfacesCultures_HC_HP |> filter(TypeCulture != 70),
  rga23_champ |> select(interview__key, Archipel_1)
) |>
  group_by(Archipel_1, TypeCultureTexte) |>
  summarize(
    `Nb Exploitants` = n_distinct(interview__key),
    `Surface (m2)` = sum(SurfaceCult, na.rm = TRUE),
    `Surface moyenne (m2)` = round(`Surface (m2)` / `Nb Exploitants`)
  )

surfacesParType_HC_HF <- rga23_surfacesCultures_HC_HP |>
  filter(TypeCulture != 70) |>
  mutate(Archipel_1 = "Total") |>
  group_by(Archipel_1, TypeCultureTexte) |>
  summarize(
    `Nb Exploitants` = n_distinct(interview__key),
    `Surface (m2)` = sum(SurfaceCult, na.rm = TRUE),
    `Surface moyenne (m2)` = round(`Surface (m2)` / `Nb Exploitants`)
  )

surfacesParTypeCultureArchipelEtTotal <- rbind(
  surfacesParType_HC_HF_Archipel,
  surfacesParType_HC_HF
) |>
  pivot_wider(names_from = c(Archipel_1), values_from = c(`Nb Exploitants`, `Surface (m2)`, `Surface moyenne (m2)`), values_fill = 0)

writeCSV(surfacesParTypeCultureArchipelEtTotal)


## Surfaces de cultures classiques par type, sexe de l'exploitant et archipel

surfacesParType_HC_HF_Archipel_sexe <- left_join(
  rga23_surfacesCultures_HC_HP |> filter(TypeCulture != 70),
  rga23_champ |> select(interview__key, Archipel_1)
) |>
  left_join(rga23_mainOeuvre |> select(interview__key, SexeChefExpl)) |>
  group_by(Archipel_1, TypeCultureTexte, SexeChefExpl) |>
  summarize(
    `Nb Exploitants` = n_distinct(interview__key),
    `Surface (m2)` = sum(SurfaceCult, na.rm = TRUE),
    `Surface moyenne (m2)` = round(`Surface (m2)` / `Nb Exploitants`)
  )

surfacesParType_HC_HF_sexe <- rga23_surfacesCultures_HC_HP |>
  filter(TypeCulture != 70) |>
  left_join(rga23_mainOeuvre |> select(interview__key, SexeChefExpl)) |>
  mutate(Archipel_1 = "Total") |>
  group_by(Archipel_1, TypeCultureTexte, SexeChefExpl) |>
  summarize(
    `Nb Exploitants` = n_distinct(interview__key),
    `Surface (m2)` = sum(SurfaceCult, na.rm = TRUE),
    `Surface moyenne (m2)` = round(`Surface (m2)` / `Nb Exploitants`)
  )

Partie3_surfacesParTypeCultureArchipelEtTotalSexe <- rbind(
  surfacesParType_HC_HF_Archipel_sexe,
  surfacesParType_HC_HF_sexe
) |>
  pivot_wider(names_from = c(Archipel_1, SexeChefExpl), values_from = c(`Nb Exploitants`, `Surface (m2)`, `Surface moyenne (m2)`), values_fill = 0)

writeCSV(Partie3_surfacesParTypeCultureArchipelEtTotalSexe)

## Jardins océaniens

Partie3_surfacesJOArchipel <- left_join(
  rga23_prodVegetales |> filter(ModesProduction__4 == 1),
  rga23_champ |> select(interview__key, Archipel_1)
) |>
  mutate(
    SurfaceBioJardins = case_when(
      SurfaceBioJardins == 1 ~ SurfaceJardins,
      TRUE ~ 0
    ),
    TypeCultureTexte = "Jardins océaniens"
  ) |>
  group_by(Archipel_1, TypeCultureTexte) |>
  summarize(
    `Nb Exploitants` = n_distinct(interview__key),
    `Surface (m2)` = sum(SurfaceJardins, na.rm = TRUE),
    `Surface moyenne (m2)` = round(`Surface (m2)` / `Nb Exploitants`),
  )

writeCSV(Partie3_surfacesJOArchipel)

ensembleSurfacesTypeJoArchipel <- rbind(surfacesParType_HC_HF_Archipel, Partie3_surfacesJOArchipel)

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
    TypeCultureTexte = "Jardins océaniens"
  ) |>
  group_by(Archipel_1, TypeCultureTexte, SexeChefExpl) |>
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
    TypeCultureTexte = "Jardins océaniens",
    Archipel_1 = "Total"
  ) |>
  group_by(Archipel_1, TypeCultureTexte, SexeChefExpl) |>
  summarize(
    `Nb Exploitants` = n_distinct(interview__key),
    `Surface (m2)` = sum(SurfaceJardins, na.rm = TRUE),
    `Surface moyenne (m2)` = round(`Surface (m2)` / `Nb Exploitants`),
  )

Partie3_surfacesJoArchipelEtTotalSexe <- rbind(
  surfacesJOArchipelSexe,
  surfacesJOSexe
) |>
  pivot_wider(names_from = c(Archipel_1, SexeChefExpl), values_from = c(`Nb Exploitants`, `Surface (m2)`, `Surface moyenne (m2)`), values_fill = 0)

writeCSV(Partie3_surfacesJoArchipelEtTotalSexe)

### Tableau pour l'encadré

surfaceCultFourrageres <- rga23_surfacesCultures |>
  filter(TypeCulture == 70) |>
  mutate(TypeCultureTexte = case_when(
    (culture_id == 701) ~ "70a - Cultures fourragères : pâturages",
    (culture_id == 702) ~ "70a - Cultures fourragères : pâturages",
    (culture_id == 703) ~ "70b - Cultures fourragères : maïs fourrage et ensilage et sorgho",
    (culture_id == 704) ~ "70b - Cultures fourragères : maïs fourrage et ensilage et sorgho",
    (culture_id == 705) ~ "70a - Cultures fourragères : pâturages",
    TRUE ~ as.character(TypeCulture)
  )) |>
  group_by(TypeCultureTexte) |>
  summarize(
    `Surface (m2)` = sum(SurfaceCult, na.rm = TRUE)
  )

encadreSAUTypeTotal <- rbind(
  surfaceCultFourrageres,
  surfacesParType_HC_HF |> ungroup() |>
    select(-Archipel_1, -`Nb Exploitants`, -`Surface moyenne (m2)`),
  surfacesJO <- rga23_prodVegetales |>
    filter(ModesProduction__4 == 1) |>
    mutate(
      TypeCultureTexte = "Jardins océaniens"
    ) |>
    group_by(TypeCultureTexte) |>
    summarize(`Surface (m2)` = sum(SurfaceJardins, na.rm = TRUE))
) |>
  arrange(TypeCultureTexte)
encadreSAUTypeTotalHa <- encadreSAUTypeTotal |>
  mutate(`Surface (Ha)` = round(`Surface (m2)` / 10000)) |>
  select(-`Surface (m2)`)
writeCSV(encadreSAUTypeTotalHa)

hectaresCulturesVegetales <- encadreSAUTypeTotal |>
  filter(TypeCultureTexte != "70a - Cultures fourragères : pâturages") |>
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

surfacesParArchipel <- rbind(
  Partie3_surfacesJOArchipel |>
    select(-`Surface moyenne (m2)`, `Nb Exploitants`),
  left_join(
    rga23_surfacesCultures_HC_HP, rga23_champ |> select(interview__key, Archipel_1)
  ) |>
    group_by(Archipel_1) |>
    summarize(
      `Surface (m2)` = sum(SurfaceCult, na.rm = TRUE)
    )
) |>
  group_by(Archipel_1) |>
  summarize(
    `Surface de productions végétales (Ha)` = round(sum(`Surface (m2)`) / 10000)
  )

jacheres <- left_join(
  rga23_surfacesCultures |>
    filter(TypeCulture == 80),
  rga23_champ |> select(interview__key, Archipel_1)
) |>
  group_by(Archipel_1) |>
  summarize(
    `dont jachères (Ha)` = round(sum(SurfaceCult, na.rm = TRUE) / 10000)
  )

avecJacheres <- left_join(surfacesParArchipel, jacheres)

