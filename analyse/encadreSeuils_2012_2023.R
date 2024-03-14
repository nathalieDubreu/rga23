source("champs/indicatricesAppartenances.R")

# 2750 unités en commun (validité au titre de la culture ou de l'élevage pour le RGA 23 et non pas du coprah)
rga23B |>
  filter(ValideRGA == 1 & Valide2012 == 1) |>
  count()

# 767 unités appartiennent uniquement au champ RGA2012
rga23B |>
  filter(is.na(ValideRGA) & Valide2012 == 1) |>
  count()
## 43 observations coté élevage
elevage12Valide <- inner_join(
  rga23B |> filter(is.na(ValideRGA) & ElevageValide2012 == 1),
  readCSV("rga23_prodAnimales.csv")
)
## 36 dû au moins à l'écart sur le nombre de ruchers
ecartRuchers <- elevage12Valide |>
  filter(replace_na(NbRuchesPourProduire, 0) + replace_na(NbRuchettes, 0) >= 20)
## 1 parce qu'il a plus de 10 équidés (mais pas assez de naissances pour respecter le seuil lié aux équidés dans le RGA 2023 - 0 en l'occurence)
## 2 avec le critères Autres volailles
## 2 avec le critère caprins
## 1 porcin
## 1 poulet de chair

## 727 observations coté cultures
culture12Valide <- inner_join(
  rga23B |> filter(is.na(ValideRGA) & CultureValide2012 == 1),
  readCSV("rga23_prodVegetales.csv")
)
## dont 701 à cause de la SAU : 1000m² en 2012
ecartSAU <- culture12Valide |>
  filter(SurfaceTotalProdAgri >= 1000)
## 26 à cause de la surface des pépinières : 500m² en 2012
ecartPepiniere <- culture12Valide |>
  filter(SurfaceTotalProdAgri < 1000 & totalSurfacePepinieres >= 500)


# 136 unités appartiennent uniquement au champ RGA2023
rga23B |>
  filter(ValideRGA == 1 & is.na(Valide2012)) |>
  count()

## 47 observations coté élevage
elevage23Valide <- inner_join(
  rga23B |> filter(is.na(Valide2012) & ElevageValideRGA == 1),
  readCSV("rga23_prodAnimales.csv")
)

# 7 ont exactement 100 poules pondeuses
# 1 qui a au moins 10 lapines mères (mais pas plus de 20 lapins de plus de 30 jours)
# 38 qui ont une truie mère (mais pas plus de 10 porcins de plus de 30 jours)
# 1 qui a 2 bovins de plus de 2 ans (mais pas plus de 10 animaux de plus de 30 jours)

## 89 observations coté cultures -> en raison des 100m² de serres
culture23Valide <- inner_join(
  rga23B |> filter(is.na(Valide2012) & CultureValideRGA == 1),
  readCSV("rga23_prodVegetales.csv")
)
validiteSerresRGA <- left_join(readCSV("rga23_surfacesCultures.csv"), readInputCSV("culturesChampRGA.csv") |> select(culture_id, idSeuilRGA), by = c("culture_id")) |>
  group_by(interview__key, idSeuilRGA) |>
  summarize(SurfaceCulturesSeuil = sum(SurfaceCult)) |>
  filter(
        # 9	Serres et abris hauts	100	m²
        idSeuilRGA == 9 & SurfaceCulturesSeuil >= 100) 
ecartSerres <- inner_join(culture23Valide, validiteSerresRGA)
