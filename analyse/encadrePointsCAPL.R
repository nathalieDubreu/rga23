source("champs/comparatifsAppartenances.R")

rga23B |>
  filter(indicRGA23 == 1) |>
  count()
# 4080
rga23B |>
  filter(PointsCAPL >= 400 | indicRGA23_Coprah == 1) |>
  count()
# 4023

rga23B |>
  filter(indicRGA23 == 1 & (PointsCAPL >= 400 | indicRGA23_Coprah == 1)) |>
  count()
# 3687 unités en commun

#####################################
# Points CAPL mais absents du RGA   #
#####################################

CAPL_nonRGA <- rga23B |>
  filter(indicRGA23 == 0 & (PointsCAPL >= 400 | indicRGA23_Coprah == 1))

CAPL_nonRGA |>
  summarize(
    somme = n(),
    pointsMax = max(PointsCAPL),
    pointsMoy = mean(PointsCAPL)
  )
# 347 unités respectent uniquement les points CAPL
## 4110 points CAPL au mx
## 638 points en moyenne

# Graines germées : 3
inner_join(
  CAPL_nonRGA,
  readCSV("rga23_surfacesCultures.csv") |>
    filter(culture_id == 122) |>
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
# 386 sont valides pour le RGA mais n'atteignent pas le nombre de points CAPL
# (ils ont 224 points en moyenne)

validiteSerresRGA <- left_join(readCSV("rga23_surfacesCultures.csv"), readInputCSV("culturesChampRGA.csv") |> select(culture_id, idSeuilRGA), by = c("culture_id")) |>
  group_by(interview__key, idSeuilRGA) |>
  summarize(SurfaceCulturesSeuil = sum(SurfaceCult)) |>
  filter(
    # 9	Serres et abris hauts	100	m²
    idSeuilRGA == 9 & SurfaceCulturesSeuil >= 100
  )
inner_join(RGA_nonCAPL, validiteSerresRGA) |>
  count()
## 214 exploitants ont des serres de 100m² ou plus

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
## 41 exploitants ont au moins une truie mère

inner_join(RGA_nonCAPL,
  readCSV("rga23_prodVegetales.csv") |> select(interview__key, SurfaceTotalProdAgri),
  by = "interview__key"
) |>
  filter(SurfaceTotalProdAgri >= 10000) |>
  count()
## 25 ont plus d'un hectare de SAU

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