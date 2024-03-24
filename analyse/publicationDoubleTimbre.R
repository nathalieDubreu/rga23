## Restriction au champ 23 du RGA
rga23_champ <- readCSV("rga23_general.csv") |>
  filter(indicRGA23 == 1) |>
  mutate(TypeExploitation = case_when(
    RaisonsRecensement__1 == 1 & RaisonsRecensement__2 == 0 & RaisonsRecensement__3 == 0 ~ "Cultivateurs seuls",
    RaisonsRecensement__1 == 0 & RaisonsRecensement__2 == 1 & RaisonsRecensement__3 == 0 ~ "Eleveurs seuls",
    RaisonsRecensement__1 == 0 & RaisonsRecensement__2 == 0 & RaisonsRecensement__3 == 1 ~ "Producteurs de coprah seuls",
    TRUE ~ "Pluriactifs parmi cultures, élevages et coprah"
  ))

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
)
rga23_cocoteraies <- inner_join(
  readCSV("rga23_cocoteraies.csv"),
  rga23_champ |> select(interview__key)
)
rga23_exploitations <- inner_join(
  readCSV("rga23_exploitations.csv"),
  rga23_champ |> select(interview__key, RaisonsRecensement__1, RaisonsRecensement__2, Archipel_1)
)
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
source("analyse/profilsExploitations.R")
source("analyse/prodVegetalesEtSurfaces.R")
source("analyse/prodAnimales.R")
source("analyse/chefExplEtMainOeuvre.R")
source("analyse/partie5_Coprahculture.R")
source("analyse/partie6_Fertilisation.R")
rmarkdown::render("analyse/publicationDoubleTimbre.Rmd", encoding = "UTF-8")

## Tableaux supplémentaires pour la publication
source("analyse/encadreSeuils_2012_2023.R")
source("analyse/Partie1_moEquivalentEtp.R")
source("analyse/donneesCadrage2012.R")
source("analyse/SAU_horsCoco_horsCultFourrag.R")
source("analyse/destinationParTaille.R")
source("analyse/encadrePointsCAPL.R")

## Pour plus tard...
source("analyse/surfacesBio_enAttenteExpertise.R")
