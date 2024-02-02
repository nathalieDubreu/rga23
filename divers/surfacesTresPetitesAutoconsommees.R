# Récupération des seuils du RGA23
culturesChampRGA <- readInputCSV("culturesChampRGA.csv") |>
  select(culture_id, idSeuilRGA) |>
  filter(!is.na(culture_id))

# Calcul des surfaces pour chacune des catégories de seuils du RGA
surfacesParCategoriesSeuil <- left_join(readCSV("rga23_surfacesCultures.csv"), culturesChampRGA, by = c("culture_id")) |>
  group_by(interview__key, idSeuilRGA) |>
  summarize(SurfaceCulturesSeuil = sum(SurfaceCult)) |>
  mutate( # 3	Pomme de terre et ensemble des racines et tubercules 3000m²
    surfaceSeuil3 = case_when((idSeuilRGA == 3) ~ SurfaceCulturesSeuil, TRUE ~ 0),
    # 4	Canne à sucre	3000m²
    surfaceSeuil4 = case_when((idSeuilRGA == 4) ~ SurfaceCulturesSeuil, TRUE ~ 0),
    # 5	Cultures fruitières (y compris bananes et ananas)	3000m²
    surfaceSeuil5 = case_when((idSeuilRGA == 5) ~ SurfaceCulturesSeuil, TRUE ~ 0),
    # 7	Ensemble PPAM, cultures ornementales et pépinières	3000m²
    surfaceSeuil7 = case_when((idSeuilRGA == 7) ~ SurfaceCulturesSeuil, TRUE ~ 0),
    # 8	Légumes frais et fraises	1000m²
    surfaceSeuil8 = case_when((idSeuilRGA == 8) ~ SurfaceCulturesSeuil, TRUE ~ 0),
    # 9	Serres et abris hauts	100m²
    surfaceSeuil9 = case_when((idSeuilRGA == 9) ~ SurfaceCulturesSeuil, TRUE ~ 0)
  ) |>
  filter(ifelse(is.na(surfaceSeuil3), 0, surfaceSeuil3) +
           ifelse(is.na(surfaceSeuil4), 0, surfaceSeuil4) +
           ifelse(is.na(surfaceSeuil5), 0, surfaceSeuil5) +
           ifelse(is.na(surfaceSeuil7), 0, surfaceSeuil7) +
           ifelse(is.na(surfaceSeuil8), 0, surfaceSeuil8) +
           ifelse(is.na(surfaceSeuil9), 0, surfaceSeuil9) > 0) |>
  group_by(interview__key) |>
  summarize(
    surfaceSeuil3 = sum(surfaceSeuil3),
    surfaceSeuil4 = sum(surfaceSeuil4),
    surfaceSeuil5 = sum(surfaceSeuil5),
    surfaceSeuil7 = sum(surfaceSeuil7),
    surfaceSeuil8 = sum(surfaceSeuil8),
    surfaceSeuil9 = sum(surfaceSeuil9)
  )

rga23_prodVegetales_classiques <- inner_join(readCSV("rga23_prodVegetales.csv"), surfacesParCategoriesSeuil)

autoConsoPetitesSurfaces <- rga23_prodVegetales_classiques |>
  filter(
    # Auto-conso totale du vivrier + pommes de terre et #3	Pomme de terre et ensemble des racines et tubercules <= 300	m²
    ((PartComVivri__1 == 100 | is.na(PartComVivri__1)) &
       (TypeCultMaraich__148 == 0 | is.na(TypeCultMaraich__148) | (TypeCultMaraich__148 == 1 & PartComMaraic__1 == 100)) & (surfaceSeuil3 > 0 & surfaceSeuil3 <= 300)) |
      # Auto-conso totale des PPAM et #4	Canne à sucre	<= 300m²
      (PartComPlantes__1 == 100 & (surfaceSeuil4 > 0 & surfaceSeuil4 <= 300)) |
      # Auto-conso totale du fruitiers et #5	Cultures fruitières (y compris bananes et ananas)	<= 300	m²
      (PartComFruit__1 == 100 & (surfaceSeuil5 > 0 & surfaceSeuil5 <= 300)) |
      # Auto-conso totale des PPAM, pépinières et fleurs et #7 Ensemble PPAM, cultures ornementales et pépinières	<= 300	m²
      ((PartComPlantes__1 == 100 | is.na(PartComPlantes__1)) &
         (PartComPepinieres__1 == 100 | is.na(PartComPepinieres__1)) &
         (PartComFlorale__1 == 100 | is.na(PartComFlorale__1)) & (surfaceSeuil7 > 0 & surfaceSeuil7 <= 300)) |
      # Auto-conso totale du maraichage + fafa et # 8	Légumes frais et fraises	<= 100	m²
      ((PartComMaraic__1 == 100 | is.na(PartComMaraic__1)) &
         (TypeCultVivrieres__202 == 0 | is.na(TypeCultVivrieres__202) | (TypeCultVivrieres__202 == 1 & PartComVivri__1)) & (surfaceSeuil8 > 0 & surfaceSeuil8 <= 100)) |
      # Auto-conso totale des PPAM, des fleurs et du maraichage et #9 Serres et abris hauts	<= 10	m²
      ((PartComPlantes__1 == 100 | is.na(PartComPlantes__1)) &
         (PartComMaraic__1 == 100 | is.na(PartComMaraic__1)) &
         (PartComFlorale__1 == 100 | is.na(PartComFlorale__1)) & (surfaceSeuil9 > 0 & surfaceSeuil9 <= 10))
  ) |>
  select(interview__key, SurfaceTotalProdAgri, totalSurfDeclarees, totalSurfaceMarai, PartComMaraic__1, totalSurfaceFruit, PartComFruit__1, totalSurfaceVivri, PartComVivri__1, totalSurfaceFlorale, PartComFlorale__1, totalSurfacePlantes, PartComPlantes__1, totalSurfacePepinieres, PartComPepinieres__1, surfaceSeuil3, surfaceSeuil4, surfaceSeuil5, surfaceSeuil7, surfaceSeuil8, surfaceSeuil9, TypeCultMaraich__148, TypeCultVivrieres__202)

maraichage <- autoConsoPetitesSurfaces |>
  filter(PartComMaraic__1 == 100 &
           ((surfaceSeuil3 > 0 & surfaceSeuil3 <= 300 & TypeCultMaraich__148 == 1) | (surfaceSeuil8 > 0 & surfaceSeuil8 <= 100) | (surfaceSeuil9 > 0 & surfaceSeuil9 <= 10))) |>
  filter((surfaceSeuil3 <= 3000 | TypeCultMaraich__148 == 0) & surfaceSeuil8 <= 1000 & surfaceSeuil9 <= 100) |>
  mutate(partSurface = round(as.numeric(totalSurfaceMarai) / as.numeric(SurfaceTotalProdAgri) * 100, 1),
         truc = "maraichage")

vivrier <- autoConsoPetitesSurfaces |>
  filter(PartComVivri__1 == 100 &
           as.numeric(totalSurfaceVivri) <= 300 &
           ((surfaceSeuil3 > 0 & surfaceSeuil3 <= 300) | (TypeCultVivrieres__202 == 1 & surfaceSeuil8 > 0 & surfaceSeuil8 <= 100))) |>
  filter(surfaceSeuil3 <= 3000 & (surfaceSeuil8 <= 1000 | TypeCultVivrieres__202 == 0)) |>
  mutate(partSurface = round(as.numeric(totalSurfaceVivri) / as.numeric(totalSurfDeclarees) * 100, 1),
         truc = "vivrier")

fruitier <- autoConsoPetitesSurfaces |>
  filter(PartComFruit__1 == 100 &
           (surfaceSeuil5 > 0 & surfaceSeuil5 <= 300)) |>
  filter(surfaceSeuil5 <= 3000) |>
  mutate(partSurface = round(as.numeric(totalSurfaceFruit) / as.numeric(totalSurfDeclarees) * 100, 1),
         truc = "fruitier")

plantes <- autoConsoPetitesSurfaces |>
  filter(PartComPlantes__1 == 100 &
           totalSurfacePlantes <= 300 &
           ((surfaceSeuil4 > 0 & surfaceSeuil4 <= 300) | (surfaceSeuil7 > 0 & surfaceSeuil7 <= 300) | (surfaceSeuil9 > 0 & surfaceSeuil9 <= 10))) |>
  filter(surfaceSeuil4 <= 3000 & surfaceSeuil7 <= 3000 & surfaceSeuil9 <= 100) |>
  mutate(partSurface = round(as.numeric(totalSurfacePlantes) / as.numeric(totalSurfDeclarees) * 100, 1),
         truc = "plantes")

fleurs <- autoConsoPetitesSurfaces |>
  filter(PartComFlorale__1 == 100 &
           totalSurfaceFlorale <= 300 &
           ((surfaceSeuil7 > 0 & surfaceSeuil7 <= 300) | (surfaceSeuil9 > 0 & surfaceSeuil9 <= 10))) |>
  filter(surfaceSeuil7 <= 3000 & surfaceSeuil9 <= 100) |>
  mutate(partSurface = round(as.numeric(totalSurfaceFlorale) / as.numeric(totalSurfDeclarees) * 100, 1),
         truc = "florales")

pepinieres <- autoConsoPetitesSurfaces |>
  filter(PartComPepinieres__1 == 100 &
           (surfaceSeuil7 > 0 & surfaceSeuil7 <= 300)) |>
  filter(surfaceSeuil7 <= 3000) |>
  mutate(partSurface = round(as.numeric(totalSurfacePepinieres) / as.numeric(SurfaceTotalProdAgri) * 100, 1),
         truc = "pepinieres")

total <- rbind(
  maraichage,
  vivrier,
  fruitier,
  plantes,
  fleurs,
  pepinieres
) |> arrange(desc(partSurface))

total |>
  summarize(n_distinct(interview__key))
# 142 au 31/01
# 75 au 01/02

# writeCSV(total)


## Fonction de suppression inutilisée finalement -> traitement au cas par cas dans l'appli de collecte
suppressionDonneesTables <- function(rga23_surfacesCultures, rga23_prodVegetales, numTypeCulture, tableTypeCulture, presenceTypeCulture, partCommercTypeCulture, totalSurfaceTypeCulture) {
  # Modifier rga23_surfacesCultures
  rga23_surfacesCultures <- anti_join(rga23_surfacesCultures, tableTypeCulture %>% mutate(TypeCulture = numTypeCulture) %>% select(interview__key, TypeCulture))
  
  # Modifier rga23_prodVegetales
  rga23_prodVegetales <- left_join(rga23_prodVegetales, tableTypeCulture %>% mutate(Surface = "null") %>% select(interview__key, Surface)) %>%
    mutate(
      {{ totalSurfaceTypeCulture }} := case_when(
        (Surface == "null") ~ NA_character_,
        TRUE ~ !!as.name(totalSurfaceTypeCulture)
      ),
      {{ partCommercTypeCulture }} := case_when(
        (Surface == "null") ~ NA_real_,
        TRUE ~ !!as.name(partCommercTypeCulture)
      ),
      {{ presenceTypeCulture }} := case_when(
        (Surface == "null") ~ "0",
        TRUE ~ !!as.name(presenceTypeCulture)
      )
    )
  
  # Retourner les tables modifiées
  return(list(rga23_surfacesCultures = rga23_surfacesCultures, rga23_prodVegetales = rga23_prodVegetales))
}

# Maraichage
# resultats <- suppressionDonneesTables(
#   rga23_surfacesCultures,
#   rga23_prodVegetales,
#   10,
#   maraichage,
#   "CulturesPresentes__10",
#   "PartComMaraic__1",
#   "totalSurfaceMarai"
# )
# rga23_surfacesCulturesB <- resultats$rga23_surfacesCultures
# rga23_prodVegetalesB <- resultats$rga23_prodVegetales
