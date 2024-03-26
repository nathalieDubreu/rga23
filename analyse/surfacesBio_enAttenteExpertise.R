# A reprendre avec la nouvelle variable définie pour la DAG ?!?

## Surface bio ou non / archipel
surfacesCulturesBioNon <- left_join(rga23_surfacesCultures, rga23_exploitations) |>
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
  ), surfaceBioDAGCult = case_when(
    (AgriBio_DAG == 1 | AgriBio_DAG == 3) & SurfaceBio == 1 ~ SurfaceCult,
    TRUE ~ 0
  )) |>
  summarize(
    `Surface BIO déclarée RGA (Ha)` = round((sum(surfaceBioCult, na.rm = TRUE) / 10000), 1),
    `Surface BIO validée DAG (Ha)` = round((sum(surfaceBioDAGCult, na.rm = TRUE) / 10000), 1),
    `Nb Exploitants de ce type de cultures` = n_distinct(interview__key),
    `Surface (Ha)` = round((sum(SurfaceCult, na.rm = TRUE) / 10000), 1)
  )

surfaceTotaleBioClassiques <- as.numeric(sum(surfacesCulturesBioNon$`Surface BIO déclarée RGA (Ha)`, na.rm = TRUE))
surfaceTotaleBioDAGlassiques <- as.numeric(sum(surfacesCulturesBioNon$`Surface BIO validée DAG (Ha)`, na.rm = TRUE))
surfaceTotaleClassiques <- as.numeric(sum(surfacesCulturesBioNon$`Surface (Ha)`, na.rm = TRUE))
nbExploitantsTotalClassiques <- as.integer(rga23_prodVegetales |> filter(ModesProduction__1 == 1) |> count())

surfacesCulturesBioNonEtTotal <- surfacesCulturesBioNon |>
  add_row(
    TypeCulture = "Total cultures classiques",
    `Surface BIO déclarée RGA (Ha)` = surfaceTotaleBioClassiques,
    `Surface BIO validée DAG (Ha)` = surfaceTotaleBioDAGlassiques,
    `Nb Exploitants de ce type de cultures` = nbExploitantsTotalClassiques,
    `Surface (Ha)` = surfaceTotaleClassiques
  )

surfacesJardinsOceaniensBioNon <- left_join(rga23_prodVegetales, rga23_exploitations) |>
  filter(ModesProduction__4 == 1) |>
  mutate(SurfaceBioJardins = case_when(
    SurfaceBioJardins == 1 ~ SurfaceJardins,
    TRUE ~ 0
  ), SurfaceBioDAGJardins = case_when(
    (AgriBio_DAG == 1 | AgriBio_DAG == 3) & SurfaceBioJardins == 1 ~ SurfaceJardins,
    TRUE ~ 0
  )) |>
  summarize(
    `Surface BIO déclarée RGA (Ha)` = round((sum(SurfaceBioJardins, na.rm = TRUE) / 10000), 1),
    `Surface BIO validée DAG (Ha)` = round((sum(SurfaceBioDAGJardins, na.rm = TRUE) / 10000), 1),
    `Nb Exploitants de ce type de cultures` = n_distinct(interview__key),
    `Surface (Ha)` = round((sum(SurfaceJardins, na.rm = TRUE) / 10000), 1)
  )

surfaceTotaleBio <- surfaceTotaleBioClassiques + surfacesJardinsOceaniensBioNon$`Surface BIO déclarée RGA (Ha)`
surfaceTotaleDAGBio <- surfaceTotaleBioDAGlassiques + surfacesJardinsOceaniensBioNon$`Surface BIO validée DAG (Ha)`
surfaceTotale <- surfaceTotaleClassiques + surfacesJardinsOceaniensBioNon$`Surface (Ha)`
nbExploitantsTotal <- as.integer(rga23_prodVegetales |> filter(ModesProduction__1 == 1 | ModesProduction__4 == 1) |> count())

surfacesBioParType <- surfacesCulturesBioNonEtTotal |>
  add_row(
    TypeCulture = "Jardins Oceaniens",
    `Surface BIO déclarée RGA (Ha)` = surfacesJardinsOceaniensBioNon$`Surface BIO déclarée RGA (Ha)`,
    `Surface BIO validée DAG (Ha)` = surfacesJardinsOceaniensBioNon$`Surface BIO validée DAG (Ha)`,
    `Nb Exploitants de ce type de cultures` = surfacesJardinsOceaniensBioNon$`Nb Exploitants de ce type de cultures`,
    `Surface (Ha)` = surfacesJardinsOceaniensBioNon$`Surface (Ha)`
  ) |>
  add_row(
    TypeCulture = "Total",
    `Surface BIO déclarée RGA (Ha)` = surfaceTotaleBio,
    `Surface BIO validée DAG (Ha)` = surfaceTotaleDAGBio,
    `Nb Exploitants de ce type de cultures` = nbExploitantsTotal,
    `Surface (Ha)` = surfaceTotale
  )
writeCSV(surfacesBioParType)
