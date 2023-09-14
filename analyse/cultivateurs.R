source("analyse/eligibles.R")

## Eligibles cultivateurs
eligiblesCultivateurs <- eligiblesRGA |>
  filter(RaisonsRecensement__1 == 1)

## SAU
eligiblesCultivateurs |>
  mutate(SAU = case_when(is.na(SurfaceTotalProdAgri) ~ 0, TRUE ~ as.numeric(SurfaceTotalProdAgri))) |>
  summarise(
    SAU_totale_hectare = sum(SAU) / 10000,
    SAU_moyenne_hectare = mean(SAU) / 10000,
    SAU_max_hectare = max(SAU) / 10000
  )

## SAU les plus faibles
eligiblesCultivateurs |>
  filter(SurfaceTotalProdAgri > 0) |>
  summarise(surfaceMin_m2 = min(SurfaceTotalProdAgri))

eligiblesCultivateurs |>
  filter(SurfaceTotalProdAgri > 0 & SurfaceTotalProdAgri < 100) |>
  summarise(nbSurfacesInf100m2 = n())

### Par type de cultures classiques

# Cultures maraîchères....................................10/10
# Cultures vivrières......................................20/20
# Cultures fruitières (hors pépinères) et bois d'oeuvre...30/30
# Feuillages et cultures florales (hors pépinières).......40/40
# Plantes aromatiques, stimulantes et médicinales.........50/50
# Pépinières (plantes vendues en pot).....................60/60
# Cultures fourragères....................................70/70
# Jachères................................................80/80
eligiblesCultivateurs |>
  filter(ModesProduction__1 == 1) |>
  summarise(
    cultMaraicheres = sum(CulturesPresentes__10),
    cultVivrieres = sum(CulturesPresentes__20),
    cultFruitieres = sum(CulturesPresentes__30),
    cultFlorales = sum(CulturesPresentes__40),
    cultPPAM = sum(CulturesPresentes__50),
    cultPepinieres = sum(CulturesPresentes__60),
    cultFourrageres = sum(CulturesPresentes__70),
    cultJacheres = sum(CulturesPresentes__80)
  )

SurfacesCultures <- readCSV("rga23_surfacesCultures.csv")

SurfacesCulturesEligibles <- inner_join(SurfacesCultures, eligiblesCultivateurs |> select(interview__key))

surfacesCultures <- SurfacesCulturesEligibles |>
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
  summarize(
    `Nombre d'exploitants` = n_distinct(interview__key),
    `Surface Totale (Ha)` = round((sum(SurfaceCult) / 10000), 1),
    `Surface moyenne (m²)` = mean(SurfaceCult),
    `Surface min (m²)` = min(SurfaceCult),
    `Surface max (m²)` = max(SurfaceCult)
  )

writeCSV(surfacesCultures)


