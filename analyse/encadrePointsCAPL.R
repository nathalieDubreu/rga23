source("champs/comparatifsAppartenances.R")

rga23B |>
  filter(indicRGA23 == 1) |>
  count()
# 4080
rga23B |>
  filter(PointsCAPL >= 400 | indicRGA23_Coprah == 1) |>
  count()
# 4304

rga23B |>
  filter(indicRGA23 == 1 & (PointsCAPL >= 400 | indicRGA23_Coprah == 1)) |>
  count()
# 3880 unités en commun

#####################################
# Points CAPL mais absents du RGA   #
#####################################

CAPL_nonRGA <- rga23B |>
  filter(indicRGA23 == 0 & (PointsCAPL >= 400 | indicRGA23_Coprah == 1))

CAPL_nonRGA |>
  summarize(
    somme = n(),
    pointsMax = max(PointsCAPL),
    pointsMoy = mean(PointsCAPL),
    pointsMedian = median(PointsCAPL)
  )
# 424 unités respectent uniquement les points CAPL
## 8370 points CAPL au mx
## 1496 points en moyenne
## 668 points médian

# Graines germées : 2
inner_join(
  CAPL_nonRGA,
  readCSV("rga23_surfacesCultures.csv") |>
    filter(culture_id == 122 & SurfaceCult >= 50) |>
    select(interview__key, SurfaceCult)
) |> count()

# Plus de 128m² de vanille sous ombrage naturel : 149
inner_join(
  CAPL_nonRGA,
  readCSV("rga23_surfacesCultures.csv") |>
    filter(culture_id == 509 & SurfaceCult >= 129) |>
    select(interview__key, SurfaceCult)
) |> count()

# Ruches : 49 éleveurs ont 20 ruches/ruchettes ou plus
inner_join(
  CAPL_nonRGA,
  readCSV("rga23_prodAnimales.csv") |>
    filter(20 * replace_na(NbRuchesPourProduire, 0) +
      20 * replace_na(NbRuchettes, 0) >= 400)
) |> count()

############################################
# Présents RGA mais points insuffisants    #
############################################

RGA_nonCAPL <- rga23B |>
  filter(indicRGA23 == 1 & PointsCAPL < 400 & indicRGA23_Coprah == 0)

RGA_nonCAPL |>
  summarize(
    somme = n(),
    pointsMax = max(PointsCAPL),
    pointsMoy = mean(PointsCAPL)
  )
# 200 sont valides pour le RGA mais n'atteignent pas le nombre de points CAPL
# (ils ont 271 points en moyenne)

validiteSerresRGA <- left_join(readCSV("rga23_surfacesCultures.csv"), readInputCSV("culturesChampRGA.csv") |> select(culture_id, idSeuilRGA), by = c("culture_id")) |>
  group_by(interview__key, idSeuilRGA) |>
  summarize(SurfaceCulturesSeuil = sum(SurfaceCult)) |>
  filter(
    # 9	Serres et abris hauts	100	m²
    idSeuilRGA == 9 & SurfaceCulturesSeuil >= 100
  )
inner_join(RGA_nonCAPL, validiteSerresRGA) |>
  count()
## 27 exploitants ont des serres de 100m² ou plus

inner_join(
  RGA_nonCAPL,
  readCSV("rga23_prodAnimales.csv") |>
    filter(
      # 12	• 1 truie mère
      replace_na(NbTruiesMaternite, 0) +
        replace_na(NbTruiesGestVides, 0) >= 1
    ),
  by = "interview__key"
) |> count()
## 42 exploitants ont au moins une truie mère

inner_join(RGA_nonCAPL,
  readCSV("rga23_prodVegetales.csv") |> select(interview__key, SurfaceTotalProdAgri),
  by = "interview__key"
) |>
  filter(SurfaceTotalProdAgri >= 10000) |>
  count()
## 22 ont plus d'un hectare de SAU

inner_join(RGA_nonCAPL,
  readCSV("rga23_prodVegetales.csv") |> select(interview__key, SurfaceJardins),
  by = "interview__key"
) |>
  filter(SurfaceJardins >= 3000) |>
  count()
## 14 ont plus de 3000m² JO

inner_join(
  RGA_nonCAPL,
  readCSV("rga23_prodAnimales.csv") |>
    filter( # 15	• 100 poules pondeuses
      replace_na(NombrePoules0, 0) +
        replace_na(NombrePoules1, 0) +
        replace_na(NombrePoules3, 0) >= 100
    ),
  by = "interview__key"
) |> count()
## 15 ont 100 poules pondeuses

validiteLegumesFraisRGA <- left_join(readCSV("rga23_surfacesCultures.csv"), readInputCSV("culturesChampRGA.csv") |> select(culture_id, idSeuilRGA), by = c("culture_id")) |>
  group_by(interview__key, idSeuilRGA) |>
  summarize(SurfaceCulturesSeuil = sum(SurfaceCult)) |>
  filter(
    # 8	Légumes frais et fraises	1000	m²
    idSeuilRGA == 8 & SurfaceCulturesSeuil >= 1000
  )
inner_join(RGA_nonCAPL, validiteLegumesFraisRGA) |>
  count()
## 28
