## Restriction au champ 23 du RGA
rga23_champ <- left_join(
  readCSV("rga23_general.csv"),
  readCSV("rga23_coprahculteurs.csv") |> select(interview__key, eligibiliteCoprah)
) |>
  left_join(readCSV("rga23_exploitations.csv") |> select(interview__key, eligibilite)) |>
  filter(indicRGA23 == 1) |>
  mutate(TypeExploitation = case_when(
    RaisonsRecensement__1 == 1 & RaisonsRecensement__2 == 0 & (eligibiliteCoprah == 0 | is.na(eligibiliteCoprah)) ~ "Cultivateurs seuls",
    RaisonsRecensement__1 == 0 & RaisonsRecensement__2 == 1 & (eligibiliteCoprah == 0 | is.na(eligibiliteCoprah)) ~ "Eleveurs seuls",
    (eligibilite == 0 | is.na(eligibilite)) & RaisonsRecensement__3 == 1 ~ "Producteurs de coprah seuls",
    TRUE ~ "Pluriactifs parmi cultures, élevages et coprah"
  )) |>
  select(-eligibilite, -eligibiliteCoprah)

Partie1_TypeExploitations <- rga23_champ |>
  group_by(TypeExploitation) |>
  count()
writeCSV(Partie1_TypesExploitations)

# Tables utiles - restreintes au champ
rga23_parcelles <- inner_join(
  readCSV("rga23_parcelles.csv"),
  rga23_champ |> select(interview__key)
)
rga23_prodVegetales <- inner_join(
  readCSV("rga23_prodVegetales.csv"),
  rga23_champ |> select(interview__key, Archipel_1, indicRGA23_Cultures, indicRGA23_Elevage, indicRGA23_Coprah, TypeExploitation, lettre_unite)
)
rga23_prodAnimales <- inner_join(
  readCSV("rga23_prodAnimales.csv"),
  rga23_champ |> select(interview__key, Archipel_1)
)
rga23_surfacesCultures <- inner_join(
  readCSV("rga23_surfacesCultures.csv"),
  rga23_champ |> select(interview__key)
)

## Surfaces de cultures hors cocoteraies et hors pâturages
rga23_surfacesCultures_HC_HP <- rga23_surfacesCultures |>
  filter(culture_id != 701 & culture_id != 702 & culture_id != 705 & culture_id != 307 & culture_id != 308 & culture_id != 309) |>
  mutate(TypeCultureTexte = case_when(
    (TypeCulture == 10) ~ "10 - Cultures maraîchères",
    (TypeCulture == 20) ~ "20 - Cultures vivrières",
    (TypeCulture == 30) ~ "30 - Cultures fruitières (hors cocoteraies)",
    (TypeCulture == 40) ~ "40 - Feuillages et cultures florales (hors pépinières)",
    (TypeCulture == 50) ~ "50 - Plantes aromatiques, stimulantes et médicinales",
    (TypeCulture == 60) ~ "60 - Pépinières (plantes vendues en pot)",
    (TypeCulture == 70) ~ "70 - Cultures fourragères (hors pâturages)",
    (TypeCulture == 80) ~ "80 - Jachères",
    TRUE ~ as.character(TypeCulture)
  ))

rga23_tape <- inner_join(
  readCSV("rga23_tape.csv"),
  rga23_champ |> select(interview__key, Archipel_1)
)
rga23_coprahculteurs <- inner_join(
  readCSV("rga23_coprahculteurs.csv"),
  rga23_champ |> select(interview__key, indicRGA23_Coprah)
) |>
  filter(eligibiliteCoprah == 1)
## Conservation des coprahculteurs eligibles (5 ont coché la case Coprahculture mais ont finalement indiqué ne pas avoir eu de production de coprah)

rga23_exploitations <- inner_join(
  readCSV("rga23_exploitations.csv"),
  rga23_champ |> select(interview__key, RaisonsRecensement__1, RaisonsRecensement__2, Archipel_1)
) |>
  filter(eligibilite == 1)
## Conservation des exploitants eligibles (27 ont coché la case Elevages ou cultures mais n'ont pas passé les filtres d'éligibilite)

rga23_coexploitants <- inner_join(
  readCSV("rga23_coexploitants.csv"),
  rga23_champ |> select(interview__key, Archipel_1)
)
rga23_moPermanenteFam <- inner_join(
  readCSV("rga23_moPermanenteFam.csv"),
  rga23_champ |> select(interview__key, Archipel_1)
)
rga23_mainOeuvre <- inner_join(
  readCSV("rga23_mainOeuvre.csv"),
  rga23_champ |> select(interview__key, Archipel_1, indicRGA23_Cultures, indicRGA23_Elevage, indicRGA23_Coprah, TypeExploitation)
) |>
  mutate(totalMAOccas = replace_na(NbFemOccasAvecLien, 0) +
    replace_na(NbFemOccasSansLien, 0) +
    replace_na(NbHomOccasAvecLien, 0) +
    replace_na(NbHomOccasSansLien, 0)) |>
  mutate(
    age = 2023 - as.numeric(substring(DateNaissChefExpl, 7, 10)),
    homme = case_when(SexeChefExpl == 1 ~ 0, SexeChefExpl == 2 ~ 1),
    femme = case_when(SexeChefExpl == 1 ~ 1, SexeChefExpl == 2 ~ 0),
    `Chefs d'exploitation par classe d'age` = case_when(
      age < 40 ~ "1 - Moins de 40 ans",
      age < 60 ~ "2 - De 40 à moins de 60 ans",
      age >= 60 ~ "3 - 60 ans et plus",
      TRUE ~ "Non réponse"
    )
  )

## Tableaux hackmd + publication
source("analyse/Respect_seuils_RGA23.R")
source("analyse/Partie2_ChefsExploitations.R")
source("analyse/Partie3_ProdVegetales.R")
source("analyse/Partie4_ProdAnimales.R")
source("analyse/Partie5_Coprahculture.R")
source("analyse/Partie6_Fertilisation.R")
rmarkdown::render("analyse/publicationDoubleTimbre.Rmd", encoding = "UTF-8")

## Tableaux supplémentaires pour la publication
source("analyse/Partie1_Exploitations_moEtp.R")
source("analyse/Partie3_ProdVegetales_destinationsParTailleExpl.R")
source("analyse/Partie4_ProdAnimales_destinations.R")
source("analyse/Encadre_Seuils_2012_2023.R")
source("analyse/Encadre_PointsCAPL.R")

## Vérifications par rapport à 2012
source("analyse/donneesCadrage2012.R")

## Pour plus tard...
# source("analyse/surfacesBio_enAttenteExpertise.R")
