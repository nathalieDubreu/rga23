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

eligiblesCultivateurs |>
  filter(ModesProduction__1 == 1 & is.na(CulturesPresentes__10))

# Cultures maraîchères....................................10/10
# Cultures vivrières......................................20/20
# Cultures fruitières (hors pépinères) et bois d'oeuvre...30/30
# Feuillages et cultures florales (hors pépinières).......40/40
# Plantes aromatiques, stimulantes et médicinales.........50/50
# Pépinières (plantes vendues en pot).....................60/60
# Cultures fourragères....................................70/70
# Jachères................................................80/80
eligiblesCultivateurs |>
  filter(ModesProduction__1 == 1 & !is.na(CulturesPresentes__10)) |>
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

# test <- eligiblesCultivateurs |>
#   filter(ModesProduction__1 == 1) |>
#   select(
#     interview__key, CulturesPresentes__10, CulturesPresentes__20,
#     CulturesPresentes__30,
#     CulturesPresentes__40,
#     CulturesPresentes__50,
#     CulturesPresentes__60,
#     CulturesPresentes__70,
#     CulturesPresentes__80
#   )

surfacesCulturesEligibles <- inner_join(readCSV("rga23_surfacesCultures.csv"), eligiblesCultivateurs |> select(interview__key))

surfacesCultures <- surfacesCulturesEligibles |>
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
    `Surface Totale (Ha)` = round((sum(SurfaceCult, na.rm = TRUE) / 10000), 1),
    `Surface moyenne (m²)` = round(mean(SurfaceCult, na.rm = TRUE), 2),
    `Surface min (m²)` = min(SurfaceCult, na.rm = TRUE),
    `Surface max (m²)` = max(SurfaceCult, na.rm = TRUE)
  )

writeCSV(surfacesCultures)

# jardins océaniens

contenuJardinsOceaniens <- eligiblesCultivateurs |>
  filter(ModesProduction__4 == 1) |>
  mutate(nbTypesCultures = CultPresentesJardins__10 +
    CultPresentesJardins__20 +
    CultPresentesJardins__30 +
    CultPresentesJardins__40 +
    CultPresentesJardins__50 +
    CultPresentesJardins__60 +
    CultPresentesJardins__70 +
    CultPresentesJardins__80) |>
  select(
    interview__key,
    interview__status,
    id_enqueteur_ech,
    SurfaceJardins,
    nbTypesCultures,
    totalSurfaceFruit,
    totalSurfaceMarai,
    totalSurfaceVivri,
    totalSurfaceFlorale,
    totalSurfacePlantes,
    CultPresentesJardins__10,
    CultPresentesJardins__20,
    CultPresentesJardins__30,
    CultPresentesJardins__40,
    CultPresentesJardins__50,
    CultPresentesJardins__60,
    CultPresentesJardins__70,
    CultPresentesJardins__80,
    CultPrincipJardins__0,
    CultPrincipJardins__1,
    CultPrincipJardins__2,
    CultPrincipJardins__3,
    CultPrincipJardins__4
  )

## 1 seul type de cultures dans le jardin océanien
aVerifier <- contenuJardinsOceaniens |>
  filter(nbTypesCultures == 1) |>
  select(!nbTypesCultures)

## Fruits (30)
aVerifier <- contenuJardinsOceaniens |>
  filter(nbTypesCultures == 1 & CultPresentesJardins__30 == 1)
## Maraichage (10)
aVerifier <- contenuJardinsOceaniens |>
  filter(nbTypesCultures == 1 & CultPresentesJardins__10 == 1)
## Vivrier (20)
aVerifier <- contenuJardinsOceaniens |>
  filter(nbTypesCultures == 1 & CultPresentesJardins__20 == 1)
## Fleurs (40)
aVerifier <- contenuJardinsOceaniens |>
  filter(nbTypesCultures == 1 & CultPresentesJardins__40 == 1)
## PPAM (50)
aVerifier <- contenuJardinsOceaniens |>
  filter(nbTypesCultures == 1 & CultPresentesJardins__50 == 1)

## Zoom maraichage qui a un seuil de 1000m²
aVerifier <- contenuJardinsOceaniens |>
  filter(SurfaceJardins < 3000 & CultPresentesJardins__10 == 1)

## Jardins océaniens avec 1 ou 2 cultures
eligiblesCultivateurs |>
  filter(ModesProduction__4 == 1 & is.na(CultPrincipJardins__2) & interview__status == 120) |>
  select(interview__key, interview__status, id_enqueteur_ech, SurfaceJardins, CultPrincipJardins__0, CultPrincipJardins__1)

## Jardins océaniens avec seulement 3 cultures et au moins 1000m²
eligiblesCultivateurs |>
  filter(ModesProduction__4 == 1 & !is.na(CultPrincipJardins__2) & is.na(CultPrincipJardins__3) & SurfaceJardins >= 1000 & interview__status == 120) |>
  select(interview__key, id_enqueteur_ech, SurfaceJardins)

# En théorie, pas de pépinières dans les jardins océaniens
contenuJardinsOceaniens |> filter(substring(CultPrincipJardins__0, 0, 1) == 6 |
  substring(CultPrincipJardins__1, 0, 1) == 6 |
  substring(CultPrincipJardins__2, 0, 1) == 6 |
  substring(CultPrincipJardins__3, 0, 1) == 6 |
  substring(CultPrincipJardins__4, 0, 1) == 6)
