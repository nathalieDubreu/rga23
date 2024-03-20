source("champs/comparatifsAppartenances.R")

rga23B |>
  filter(ValideRGA == 1) |>
  count()
# 2886 (validité au titre de la culture ou de l'élevage pour le RGA 23)
rga23B |>
  filter(PointsCAPL >= 400) |>
  count()
# 2825

rga23B |>
  filter(ValideRGA == 1 & PointsCAPL >= 400) |>
  count()
# 2467 unités en commun

#####################################
# Points CAPL mais absents du RGA   #
#####################################

rga23B |>
  filter(is.na(ValideRGA) & PointsCAPL >= 400) |>
  summarize(
    somme = n(),
    pointsMax = max(PointsCAPL),
    pointsMoy = mean(PointsCAPL)
  )
# 358 unités respectent uniquement les points CAPL
## 4110 points CAPL au mx
## 638 points en moyenne
CAPL_nonRGA <- rga23B |>
  filter(is.na(ValideRGA) & PointsCAPL >= 400)

# Graines germées : 3
inner_join(
  CAPL_nonRGA,
  readCSV("rga23_surfacesCultures.csv") |>
    filter(culture_id == 122) |>
    select(interview__key, SurfaceCult)
) |> count()

# Ruches : 53 éleveurs ont 20 ruches/ruchettes ou plus
inner_join(
  CAPL_nonRGA,
  readCSV("rga23_prodAnimales.csv") |>
    filter(20 * replace_na(NbRuchesPourProduire, 0) +
      20 * replace_na(NbRuchettes, 0) >= 400)
) |> count()


############################################
# Présents RGA mais points insuffisants    #
############################################

rga23B |>
  filter(ValideRGA == 1 & PointsCAPL < 400) |>
  summarize(
    somme = n(),
    pointsMax = max(PointsCAPL),
    pointsMoy = mean(PointsCAPL)
  )
# 419 sont valides pour le RGA mais n'atteignent pas le nombre de points CAPL (ils ont 226 points en moyenne)

RGA_nonCAPL <- rga23B |>
  filter(ValideRGA == 1 & PointsCAPL < 400)

validiteSerresRGA <- left_join(readCSV("rga23_surfacesCultures.csv"), readInputCSV("culturesChampRGA.csv") |> select(culture_id, idSeuilRGA), by = c("culture_id")) |>
  group_by(interview__key, idSeuilRGA) |>
  summarize(SurfaceCulturesSeuil = sum(SurfaceCult)) |>
  filter(
    # 9	Serres et abris hauts	100	m²
    idSeuilRGA == 9 & SurfaceCulturesSeuil >= 100) 
inner_join(RGA_nonCAPL, validiteSerresRGA) |> 
  count()
## 222 exploitants ont des serres de 100m² ou plus

inner_join(
  RGA_nonCAPL,
  readCSV("rga23_prodAnimales.csv") |>
    filter(
      # 12	• 1 truie mère
      replace_na(NbTruiesMaternite, 0) +
        replace_na(NbTruiesGestVides, 0) >= 1)
) |> count()
## 50 exploitants ont au moins une truie mère