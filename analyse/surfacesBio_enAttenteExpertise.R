# A reprendre avec la nouvelle variable définie pour la DAG ?!?

## Surface bio ou non / archipel
surfacesCulturesBioNon <- rga23_surfacesCultures |>
  mutate(TypeCulture = case_when(
    (TypeCulture == 10) ~ "10 - Cultures maraîchères",
    (TypeCulture == 20) ~ "20 - Cultures vivrières",
    (TypeCulture == 30) ~ "30 - Cultures fruitières (hors pépinères) et bois d'oeuvre",
    (TypeCulture == 40) ~ "40 - Feuillages et cultures florales (hors pépinières)",
    (TypeCulture == 50) ~ "50 - Plantes aromatiques, stimulantes et médicinales",
    (TypeCulture == 60) ~ "60 - Pépinières (plantes vendues en pot)",
    (TypeCulture == 70) ~ "70 - Cultures fourragères",
    (TypeCulture == 80) ~ "80 - Jachères",
    TRUE ~ as.character(TypeCulture)
  )) |>
  group_by(TypeCulture) |>
  mutate(surfaceBioCult = case_when(
    SurfaceBio == 1 ~ SurfaceCult,
    TRUE ~ 0
  )) |>
  summarize(
    `Nb Exploitants` = n_distinct(interview__key),
    `Surface BIO (Ha)` = round((sum(surfaceBioCult, na.rm = TRUE) / 10000), 1),
    `Surface (Ha)` = round((sum(SurfaceCult, na.rm = TRUE) / 10000), 1),
    `Surface moyenne (m²)` = round(mean(SurfaceCult, na.rm = TRUE), 0)
  )

surfaceTotaleBioClassiques <- as.numeric(sum(surfacesCulturesBioNon$`Surface BIO (Ha)`, na.rm = TRUE))
surfaceTotaleClassiques <- as.numeric(sum(surfacesCulturesBioNon$`Surface (Ha)`, na.rm = TRUE))
nbExploitantsTotalClassiques <- as.integer(rga23_prodVegetales |> filter(ModesProduction__1 == 1) |> count())

surfacesCulturesBioNonEtTotal <- surfacesCulturesBioNon |>
  add_row(
    TypeCulture = "Total cultures classiques",
    `Nb Exploitants` = nbExploitantsTotalClassiques,
    `Surface BIO (Ha)` = surfaceTotaleBioClassiques,
    `Surface (Ha)` = surfaceTotaleClassiques,
    `Surface moyenne (m²)` = as.numeric(NA)
  )

surfacesJardinsOceaniensBioNon <- rga23_prodVegetales |>
  filter(ModesProduction__4 == 1) |>
  mutate(SurfaceBioJardins = case_when(
    SurfaceBioJardins == 1 ~ SurfaceJardins,
    TRUE ~ 0
  )) |>
  summarize(
    `Nb Exploitants` = n_distinct(interview__key),
    `Surface Bio (Ha)` = round((sum(SurfaceBioJardins, na.rm = TRUE) / 10000), 1),
    `Surface (Ha)` = round((sum(SurfaceJardins, na.rm = TRUE) / 10000), 1),
    `Surface moyenne (m²)` = round(mean(SurfaceJardins, na.rm = TRUE), 0)
  )

surfaceTotaleBio <- surfaceTotaleBioClassiques + surfacesJardinsOceaniensBioNon$`Surface Bio (Ha)`
surfaceTotale <- surfaceTotaleClassiques + surfacesJardinsOceaniensBioNon$`Surface (Ha)`
nbExploitantsTotal <- as.integer(rga23_prodVegetales |> filter(ModesProduction__1 == 1 | ModesProduction__4 == 1) |> count())

surfacesCulturesClassEtOceaniens <- surfacesCulturesBioNonEtTotal |>
  add_row(
    TypeCulture = "Jardins Oceaniens",
    `Nb Exploitants` = surfacesJardinsOceaniensBioNon$`Nb Exploitants`,
    `Surface BIO (Ha)` = surfacesJardinsOceaniensBioNon$`Surface Bio (Ha)`,
    `Surface (Ha)` = surfacesJardinsOceaniensBioNon$`Surface (Ha)`,
    `Surface moyenne (m²)` = surfacesJardinsOceaniensBioNon$`Surface moyenne (m²)`
  ) |>
  add_row(
    TypeCulture = "Total",
    `Nb Exploitants` = nbExploitantsTotal,
    `Surface BIO (Ha)` = surfaceTotaleBio,
    `Surface (Ha)` = surfaceTotale,
    `Surface moyenne (m²)` = as.numeric(NA)
  )
