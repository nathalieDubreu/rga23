source("champs/comparatifsAppartenances.R")

Encadre2012_ValiditeEnsembles <- rga23B |>
  summarize(
    ## RGA23
    `Valide Elevage RGA 2023` = sum(indicRGA23_Elevage == 1, na.rm = TRUE),
    `Valide Cultures RGA 2023` = sum(indicRGA23_Cultures == 1, na.rm = TRUE),
    `Valide RGA 2023 (hors coprah)` = sum(ValideRGA == 1, na.rm = TRUE),
    `Valide Coprah Uniquement RGA 2023` = sum(indicRGA23_Coprah == 1 & is.na(ValideRGA), na.rm = TRUE),
    ## RGA12
    `Valide Elevage RGA 2012` = sum(indicRGA12_Elevage == 1, na.rm = TRUE),
    `Valide Cultures RGA 2012` = sum(indicRGA12_Cultures == 1, na.rm = TRUE),
    `Valide RGA 2012` = sum(Valide2012 == 1, na.rm = TRUE)
  )
# 3517 valides RGA12
# 2886 valides RGA23 - hors coprah
writeCSV(Encadre2012_ValiditeEnsembles)

Encadre2012_ValiditeArchipel <- rga23B |>
  group_by(Archipel_1) |>
  summarize(
    `Valide RGA 2012` = sum(Valide2012 == 1, na.rm = TRUE),
    `Valide RGA 2023` = sum(ValideRGA == 1, na.rm = TRUE)
  )
writeCSV(Encadre2012_ValiditeArchipel)

Encadre2012_CroisementEnsembles <- rga23B |>
  mutate(
    `RGA 2023 - hors coprah` = case_when(
      ValideRGA == 1 ~ "Valides",
      TRUE ~ "non"
    ),
    `RGA 2012` = case_when(
      Valide2012 == 1 ~ "Valides",
      TRUE ~ "non"
    ),
  ) |>
  filter(Valide2012 == 1 | ValideRGA == 1) |>
  group_by(`RGA 2023 - hors coprah`, `RGA 2012`) |>
  count()
writeCSV(Encadre2012_CroisementEnsembles)
# 2750 unités en commun (validité au titre de la culture ou de l'élevage pour le RGA 23 et non pas du coprah)
# 767 unités appartiennent uniquement au champ RGA 2012
# 136 unités appartiennent uniquement au champ RGA 2023


elevage12Valide <- inner_join(
  rga23B |> filter(is.na(ValideRGA) & ElevageValide2012 == 1),
  readCSV("rga23_prodAnimales.csv")
)
## 43 observations coté élevage
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
    idSeuilRGA == 9 & SurfaceCulturesSeuil >= 100
  )
ecartSerres <- inner_join(culture23Valide, validiteSerresRGA)
