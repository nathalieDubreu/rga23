source("champs/champRGA.R")

# Eleveurs - nombre moyen d'oeufs par poule
## Normalement se calcule par rapport à l'effectif moyen annuel.
## Dans le cadre du RGA : nombre de poules le jour du passage et nombre d'oeufs l'année écoulée



# Consignes aux enquêteurs : ne pas relever les petites surfaces liées à la seule autoconsommation
## Que faire des surfaces présentes malgré la consigne ?
##  Exemple : un apiculteur qui a qq surfaces de cultures 38-05-33-77 ou un coprahculteur (07-77-19-56) avec 4m² de cultures mais qq dons donc surfaces déclarées…

culturesChampRGA <- readInputCSV("culturesChampRGA.csv") |> select(culture_id, idSeuilRGA)
rga23_surfacesCultures <- readCSV("rga23_surfacesCultures.csv")

cultivateursInferieurs10Champ <- left_join(rga23_surfacesCultures, culturesChampRGA, by = c("culture_id")) |>
  group_by(interview__key, idSeuilRGA) |>
  summarize(SurfaceCulturesSeuil = sum(SurfaceCult)) |>
  mutate( # 3	Pomme de terre et ensemble des racines et tubercules	<= 300	m²
    surfaceSeuil3 = case_when((idSeuilRGA == 3 & SurfaceCulturesSeuil <= 300) ~ SurfaceCulturesSeuil, TRUE ~ 0),
    # 4	Canne à sucre	3000	m²
    surfaceSeuil4 = case_when((idSeuilRGA == 4 & SurfaceCulturesSeuil <= 300) ~ SurfaceCulturesSeuil, TRUE ~ 0),
    # 5	Cultures fruitières (y compris bananes et ananas)	<= 300	m²
    surfaceSeuil5 = case_when((idSeuilRGA == 5 & SurfaceCulturesSeuil <= 300) ~ SurfaceCulturesSeuil, TRUE ~ 0),
    # 7	Ensemble PPAM, cultures ornementales et pépinières	<= 300	m²
    surfaceSeuil7 = case_when((idSeuilRGA == 7 & SurfaceCulturesSeuil <= 300) ~ SurfaceCulturesSeuil, TRUE ~ 0),
    # 8	Légumes frais et fraises	<= 100	m²
    surfaceSeuil8 = case_when((idSeuilRGA == 8 & SurfaceCulturesSeuil <= 100) ~ SurfaceCulturesSeuil, TRUE ~ 0),
    # 9	Serres et abris hauts	<= 10	m²
    surfaceSeuil9 = case_when((idSeuilRGA == 9 & SurfaceCulturesSeuil <= 10) ~ SurfaceCulturesSeuil, TRUE ~ 0)
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

cultivateursInferieurs10Champ_champ <- inner_join(cultivateursInferieurs10Champ, idExploitantsDansLeChamp)
# 470

rga23_prodVegetales <- inner_join(readCSV("rga23_prodVegetales.csv"), cultivateursInferieurs10Champ_champ)
autoConsoPetitesSurfaces <- rga23_prodVegetales |>
  filter(
    # Auto-conso totale du vivrier et #3	Pomme de terre et ensemble des racines et tubercules <= 300	m²
    ((PartComVivri__1 == 100 | is.na(PartComVivri__1)) & (TypeCultMaraich__148 == 0 | is.na(TypeCultMaraich__148) | (TypeCultMaraich__148 == 1 & PartComMaraic__1)) & surfaceSeuil3 > 0) |
      # Auto-conso totale des PPAM et #4	Canne à sucre	<= 300m²
      (PartComPlantes__1 == 100 & surfaceSeuil4 > 0) |
      # Auto-conso totale du fruitiers et #5	Cultures fruitières (y compris bananes et ananas)	<= 300	m²
      (PartComFruit__1 == 100 & surfaceSeuil5 > 0) |
      # Auto-conso totale des PPAM, pépinières et fleurs et #7 Ensemble PPAM, cultures ornementales et pépinières	<= 300	m²
      ((PartComPlantes__1 == 100 | is.na(PartComPlantes__1)) &
        (PartComPepinieres__1 == 100 | is.na(PartComPepinieres__1)) &
        (PartComFlorale__1 == 100 | is.na(PartComFlorale__1)) & surfaceSeuil7 > 0) |
      # Auto-conso totale du maraichage et # 8	Légumes frais et fraises	<= 100	m²
      (PartComMaraic__1 == 100 & surfaceSeuil8 > 0) |
      # Auto-conso totale des PPAM, des fleurs et du maraichage et #9 Serres et abris hauts	<= 10	m²
      ((PartComPlantes__1 == 100 | is.na(PartComPlantes__1)) &
        (PartComMaraic__1 == 100 | is.na(PartComMaraic__1)) &
        (PartComFlorale__1 == 100 | is.na(PartComFlorale__1)) & surfaceSeuil9 > 0)
  ) |>
  select(interview__key, SurfaceTotalProdAgri, totalSurfaceMarai, PartComMaraic__1, totalSurfaceFruit, PartComFruit__1, totalSurfaceVivri, PartComVivri__1, totalSurfaceFlorale, PartComFlorale__1, totalSurfacePlantes, PartComPlantes__1, totalSurfacePepinieres, PartComPepinieres__1)

autoConsoPetitesSurfaces_champ <- inner_join(autoConsoPetitesSurfaces, idExploitantsDansLeChamp)

# Jardins océaniens : seuil de 3000m²
## Si la surface fait moins, faut-il la répartir entre les différents types de cultures présents pour voir si l’un ou l’autre des seuils pourra être atteint ?
## Exemple : un agriculteur dispose d'un jardin océanien de 1500m² avec du maraichage, du fruitier et du vivrier.
## Il a également 3 parcelles distinctes avec chacun de ces types de cultures.
## Le fait d’additionner 1/3 de la surface du jardin avec chacun des 3 types de cultures permettrait peut-être de dépasser le seuil du maraichage par exemple.
## Ou au contraire, considérer en cas de jardins océaniens de 1500m² par exemple que s'il y a plus de 4500m² d'autres cultures, on considère le seuil atteint ?
