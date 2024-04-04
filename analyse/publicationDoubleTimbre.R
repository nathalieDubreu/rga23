rga23_complet <- left_join(
  readCSV("rga23_general.csv"),
  readCSV("rga23_coprahculteurs.csv") |> select(interview__key, eligibiliteCoprah, InterruptionTemporaireCoprah)
) |>
  left_join(readCSV("rga23_exploitations.csv") |> select(interview__key, eligibilite))

Partie1_comptages <- rga23_complet |> summarize(
  nombreTotalInterrogations = n(),
  questionnairesComplets = sum(ifelse(eligibilite == 1 | (eligibiliteCoprah == 1 & (lettre_unite == "C" | lettre_unite == "X")), 1, 0), na.rm = TRUE)
)
writeCSV(Partie1_comptages)

Partie5_comptagesCoprah <- rga23_complet |> summarize(
  coprahPlus2t7 = sum(ifelse(lettre_unite == "C" | lettre_unite == "X", 1, 0)),
  coprahInjoignablesInconnus = round(sum(ifelse((lettre_unite == "C" | lettre_unite == "X") & (statut_collecte == 4 | statut_collecte == 3 | statut_collecte == 2), 1, 0)) /
    coprahPlus2t7 * 100, 1),
  coprahDoublons = round(sum(ifelse((lettre_unite == "C" | lettre_unite == "X") & statut_collecte == 5, 1, 0)) /
    coprahPlus2t7 * 100, 1),
  coprahInterroges = sum(ifelse((lettre_unite == "C" | lettre_unite == "X") & statut_collecte == 1, 1, 0)),
  coprahArret = round(sum(ifelse((lettre_unite == "C" | lettre_unite == "X") & statut_collecte == 1 & InterruptionTemporaireCoprah == 2, 1, 0), na.rm = TRUE) /
    coprahInterroges * 100, 1)
)
writeCSV(Partie5_comptagesCoprah)
 
## Case cochée et "confirmée" i.e. répondant éligibles dans ce domaine
Partie1_casesCochees <- rga23_complet |> summarize(
  caseCultureCochee = sum(ifelse(indicRGA23 == 1 & RaisonsRecensement__1 == 1 & eligibilite == 1, 1, 0)),
  caseElevageCochee = sum(ifelse(indicRGA23 == 1 & RaisonsRecensement__2 == 1 & eligibilite == 1, 1, 0)),
  caseCoprahCochee = sum(ifelse(indicRGA23 == 1 & RaisonsRecensement__3 == 1 & eligibiliteCoprah == 1, 1, 0)),
  proportionCultureCocheeUniquement = round(sum(ifelse(indicRGA23 == 1 & RaisonsRecensement__1 == 1 & eligibilite == 1 & (RaisonsRecensement__1 + RaisonsRecensement__2 + RaisonsRecensement__3 == 1), 1, 0)) / caseCultureCochee * 100),
  proportionElevageCocheeUniquement = round(sum(ifelse(indicRGA23 == 1 & RaisonsRecensement__2 == 1 & eligibilite == 1 & (RaisonsRecensement__1 + RaisonsRecensement__2 + RaisonsRecensement__3 == 1), 1, 0)) / caseElevageCochee * 100),
  proportionCoprahCocheeUniquement = round(sum(ifelse(indicRGA23 == 1 & RaisonsRecensement__3 == 1 & eligibiliteCoprah == 1 & (RaisonsRecensement__1 + RaisonsRecensement__2 + RaisonsRecensement__3 == 1), 1, 0)) / caseCoprahCochee * 100),
)
writeCSV(Partie1_casesCochees)

## Restriction au champ 23 du RGA
rga23_champ <- rga23_complet |>
  filter(indicRGA23 == 1) |>
  select(-eligibilite, -eligibiliteCoprah, -InterruptionTemporaireCoprah)

# Tables utiles - restreintes au champ
rga23_parcelles <- inner_join(
  readCSV("rga23_parcelles.csv"),
  rga23_champ |> select(interview__key)
)
rga23_prodVegetales <- inner_join(
  readCSV("rga23_prodVegetales.csv"),
  rga23_champ |> select(interview__key, Archipel_1, indicRGA23_Cultures, indicRGA23_Elevage, indicRGA23_Coprah, lettre_unite)
)
rga23_prodAnimales <- inner_join(
  readCSV("rga23_prodAnimales.csv"),
  rga23_champ |> select(interview__key, Archipel_1, lettre_unite)
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
rga23_cocoteraies <- inner_join(
  readCSV("rga23_cocoteraies.csv"),
  rga23_champ |> select(interview__key)
)

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
  rga23_champ |> select(interview__key, Archipel_1, indicRGA23_Cultures, indicRGA23_Elevage, indicRGA23_Coprah)
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
source("analyse/Partie1_MaterielUtilise.R")
source("analyse/Partie3_ProdVegetales_destinationsParTailleExpl.R")
source("analyse/Partie4_ProdAnimales_destinations.R")
source("analyse/Encadre_Seuils_2012_2023.R")
source("analyse/Encadre_PointsCAPL.R")

## Vérifications par rapport à 2012
source("analyse/donneesCadrage2012.R")

## Pour plus tard...
source("analyse/surfacesBio_enAttenteExpertise.R")
