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

# Graines germées
inner_join(
  CAPL_nonRGA,
  readCSV("rga23_surfacesCultures.csv") |>
    filter(culture_id == 122) |>
    select(interview__key, SurfaceCult)
) |> count()

# Chevaux


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


