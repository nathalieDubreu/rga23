source("champs/champRGA.R")

# Eleveurs - nombre moyen d'oeufs par poule
## Normalement se calcule par rapport à l'effectif moyen annuel.
## Dans le cadre du RGA : nombre de poules le jour du passage et nombre d'oeufs l'année écoulée

### Vu avec Valérie, pas d'apurement

# Consignes aux enquêteurs : ne pas relever les petites surfaces liées à la seule autoconsommation
## Que faire des surfaces présentes malgré la consigne ?
##  Exemple : un apiculteur qui a qq surfaces de cultures 38-05-33-77 ou un coprahculteur (07-77-19-56) avec 4m² de cultures mais qq dons donc surfaces déclarées…

culturesChampRGA <- readInputCSV("culturesChampRGA.csv") |> select(culture_id, idSeuilRGA)
rga23_surfacesCultures <- readCSV("rga23_surfacesCultures.csv")

cultivateursSurfacesParSeuil <- left_join(rga23_surfacesCultures, culturesChampRGA, by = c("culture_id")) |>
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

rga23_prodVegetales_classiques <- inner_join(readCSV("rga23_prodVegetales.csv"), cultivateursSurfacesParSeuil)

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
  select(interview__key, SurfaceTotalProdAgri, totalSurfaceMarai, PartComMaraic__1, totalSurfaceFruit, PartComFruit__1, totalSurfaceVivri, PartComVivri__1, totalSurfaceFlorale, PartComFlorale__1, totalSurfacePlantes, PartComPlantes__1, totalSurfacePepinieres, PartComPepinieres__1, surfaceSeuil3, surfaceSeuil4, surfaceSeuil5, surfaceSeuil7, surfaceSeuil8, surfaceSeuil9, TypeCultMaraich__148, TypeCultVivrieres__202)

autoConsoPetitesSurfaces_champ <- inner_join(autoConsoPetitesSurfaces, idExploitantsDansLeChamp)

maraichage <- autoConsoPetitesSurfaces |>
  filter(PartComMaraic__1 == 100 &
    ((surfaceSeuil3 > 0 & surfaceSeuil3 <= 300 & TypeCultMaraich__148 == 1) | (surfaceSeuil8 > 0 & surfaceSeuil8 <= 100) | (surfaceSeuil9 > 0 & surfaceSeuil9 <= 10))) |>
  filter((surfaceSeuil3 <= 3000 | TypeCultMaraich__148 == 0) & surfaceSeuil8 <= 1000 & surfaceSeuil9 <= 100)

vivrier <- autoConsoPetitesSurfaces |>
  filter(PartComVivri__1 == 100 &
    totalSurfaceVivri <= 300 &
    ((surfaceSeuil3 > 0 & surfaceSeuil3 <= 300) | (TypeCultVivrieres__202 == 1 & surfaceSeuil8 > 0 & surfaceSeuil8 <= 100))) |>
  filter(surfaceSeuil3 <= 3000 & (surfaceSeuil8 <= 1000 | TypeCultVivrieres__202 == 0))

fruitier <- autoConsoPetitesSurfaces |>
  filter(PartComFruit__1 == 100 &
    (surfaceSeuil5 > 0 & surfaceSeuil5 <= 300)) |>
  filter(surfaceSeuil5 <= 3000)

plantes <- autoConsoPetitesSurfaces |>
  filter(PartComPlantes__1 == 100 &
    totalSurfacePlantes <= 300 &
    ((surfaceSeuil4 > 0 & surfaceSeuil4 <= 300) | (surfaceSeuil7 > 0 & surfaceSeuil7 <= 300) | (surfaceSeuil9 > 0 & surfaceSeuil9 <= 10))) |>
  filter(surfaceSeuil4 <= 3000 & surfaceSeuil7 <= 3000 & surfaceSeuil9 <= 100)

fleurs <- autoConsoPetitesSurfaces |>
  filter(PartComFlorale__1 == 100 &
    totalSurfaceFlorale <= 300 &
    ((surfaceSeuil7 > 0 & surfaceSeuil7 <= 300) | (surfaceSeuil9 > 0 & surfaceSeuil9 <= 10))) |>
  filter(surfaceSeuil7 <= 3000 & surfaceSeuil9 <= 100)

pepinieres <- autoConsoPetitesSurfaces |>
  filter(PartComPepinieres__1 == 100 &
           (surfaceSeuil7 > 0 & surfaceSeuil7 <= 300)) |>
  filter(surfaceSeuil7 <= 3000)

total <- rbind(maraichage, 
               vivrier,
               fruitier,
               plantes,
               fleurs,
               pepinieres) |> group_by(interview__key) |> count()

dansLeChamp <- inner_join(total, idExploitantsDansLeChamp)

# SAU inconnue
sauInconnue <- rga23 |> filter(is.na(SurfaceTotalProdAgri) & (sommeSurfaces > 0 | !is.na(totalSurfDeclarees))) |>
  select(interview__key, SurfaceTotalProdAgri, sommeSurfaces, totalSurfDeclarees)

# Jardins océaniens / Permaculture

### Uniquement du maraichage

queMaraichage <- anti_join(rga23 |>
  mutate(nbTypesCultures = CultPresentesJardins__10 +
           CultPresentesJardins__20 +
           CultPresentesJardins__30 +
           CultPresentesJardins__40 +
           CultPresentesJardins__50 +
           CultPresentesJardins__60 +
           CultPresentesJardins__70 +
           CultPresentesJardins__80) |>
  filter(SurfaceJardins >= 1000 & nbTypesCultures == 1 & CultPresentesJardins__10 == 1) |>
  select(interview__key, SurfaceTotalProdAgri, sommeSurfaces, totalSurfDeclarees, SurfaceJardins), idExploitantsDansLeChamp)

### seuil de 3000m²
## Si la surface fait moins, faut-il la répartir entre les différents types de cultures présents pour voir si l’un ou l’autre des seuils pourra être atteint ?
## Exemple : un agriculteur dispose d'un jardin océanien de 1500m² avec du maraichage, du fruitier et du vivrier.
## Il a également 3 parcelles distinctes avec chacun de ces types de cultures.
## Le fait d’additionner 1/3 de la surface du jardin avec chacun des 3 types de cultures permettrait peut-être de dépasser le seuil du maraichage par exemple.
## Ou au contraire, considérer en cas de jardins océaniens de 1500m² par exemple que s'il y a plus de 4500m² d'autres cultures, on considère le seuil atteint ?

jardinsOcenaniensTropPetits <- rga23 |> filter(SurfaceJardins < 3000 & SurfaceJardins > 0 )
# 148

jardinsOcenaniensOUT <- anti_join(jardinsOcenaniensTropPetits,idExploitantsDansLeChamp) 
# 119

avecCulturesClassiques <- jardinsOcenaniensOUT |> 
  filter(ModesProduction__1 == 1 & (SurfaceTotalProdAgri > 3000 | totalSurfDeclarees > 3000)) |>
  select(interview__key, SurfaceTotalProdAgri, sommeSurfaces, totalSurfDeclarees, SurfaceJardins)
# 14 font également de la culture classique et ont une SAU (ou total surf déclarées) > 3000m²
