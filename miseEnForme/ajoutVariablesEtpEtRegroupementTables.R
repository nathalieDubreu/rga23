library(testthat)

# ETAPE 1 - Sauvegarde des variables d'effectifs et d'ETP de la population active agricole

## Ajout des variables relatives aux ETP présentes dans la table main d'oeuvre

rga23_mainOeuvreETP <- readCSV("rga23_mainOeuvre.csv") |>
  mutate(
    ChefETP = case_when(
      (TpsTravailChefExpl == 1) ~ 0.25,
      (TpsTravailChefExpl == 2) ~ 0.5,
      (TpsTravailChefExpl == 3) ~ 0.75,
      (TpsTravailChefExpl == 4) ~ 1,
      (is.na(TpsTravailChefExpl)) ~ 1
    ),
    NbMoPermNonFam = ifelse(!is.na(nbFemMONonFamPerm__1), nbFemmesNFPerm, 0) +
      ifelse(!is.na(nbHommesNFPerm), nbHommesNFPerm, 0),
    MoPermNonFamETP =
      ifelse(!is.na(nbFemMONonFamPerm__1), nbFemMONonFamPerm__1 * 0.25, 0) +
        ifelse(!is.na(nbFemMONonFamPerm__2), nbFemMONonFamPerm__2 * 0.5, 0) +
        ifelse(!is.na(nbFemMONonFamPerm__3), nbFemMONonFamPerm__3 * 0.75, 0) +
        ifelse(!is.na(nbFemMONonFamPerm__4), nbFemMONonFamPerm__4, 0) +
        ifelse(!is.na(nbHomMONonFamPerm__1), nbHomMONonFamPerm__1 * 0.25, 0) +
        ifelse(!is.na(nbHomMONonFamPerm__2), nbHomMONonFamPerm__2 * 0.5, 0) +
        ifelse(!is.na(nbHomMONonFamPerm__3), nbHomMONonFamPerm__3 * 0.75, 0) +
        ifelse(!is.na(nbHomMONonFamPerm__4), nbHomMONonFamPerm__4, 0),
    NbMOOccasionnelle = replace_na(NbFemOccasAvecLien, 0) +
      replace_na(NbFemOccasSansLien, 0) +
      replace_na(NbHomOccasAvecLien, 0) +
      replace_na(NbHomOccasSansLien, 0),
    MOOccasionnelleETP = case_when(
      UniteDureeMOOccas == 2 ~ 8 * DureeMOOccas,
      UniteDureeMOOccas == 3 ~ 8 * 5 * DureeMOOccas,
      UniteDureeMOOccas == 4 ~ 8 * 5 * 52 / 12 * DureeMOOccas,
      TRUE ~ DureeMOOccas
    ) / 1600
  )

## Calcul ETP des coexploitants

rga23_coexploitantsETP <- readCSV("rga23_coexploitants.csv") |>
  mutate(CoexploitantsETP = case_when(
    (TempsTravailCoExploit == 1) ~ 0.25,
    (TempsTravailCoExploit == 2) ~ 0.5,
    (TempsTravailCoExploit == 3) ~ 0.75,
    (TempsTravailCoExploit == 4) ~ 1,
    (is.na(TempsTravailCoExploit)) ~ 1
  )) |>
  group_by(interview__key) |>
  summarize(
    NbCoexploitants = n(),
    CoexploitantsETP = sum(CoexploitantsETP)
  )

## Calcul ETP de la MO Permanente familiale

rga23_moPermanenteFamETP <- readCSV("rga23_moPermanenteFam.csv") |>
  mutate(MOPermFamETP = case_when(
    (TempsTravailMOFamPerm == 1) ~ 0.25,
    (TempsTravailMOFamPerm == 2) ~ 0.5,
    (TempsTravailMOFamPerm == 3) ~ 0.75,
    (TempsTravailMOFamPerm == 4) ~ 1,
    (is.na(TempsTravailMOFamPerm)) ~ 1
  )) |>
  group_by(interview__key) |>
  summarize(
    NbMOPermFam = n(),
    MOPermFamETP = sum(MOPermFamETP)
  )

## Regroupement des différentes variables ETP
rga23_etp <- left_join(rga23_mainOeuvreETP |> select(interview__key, ChefETP, NbMoPermNonFam, MoPermNonFamETP, NbMOOccasionnelle, MOOccasionnelleETP),
  rga23_coexploitantsETP,
  by = "interview__key"
) |>
  left_join(rga23_moPermanenteFamETP,
    by = "interview__key"
  ) |>
  mutate(across(where(is.numeric), ~ coalesce(., 0)))

## TEST sur les exploitations eligibles au RGA23

testUnitaireETP <- left_join(rga23_etp, readCSV("rga23_general.csv")) |> filter(indicRGA23 == 1)

expected_values <- c(
  TotalChefETP = 2628,
  TotalNbCoexploitants = 802,
  TotalCoexploitantsETP = 460,
  TotalNbMOPermFam = 1753,
  TotalMOPermFamETP = 1052,
  TotalNbMoPermNonFam = 935,
  TotalMoPermNonFamETP = 729,
  TotalNbMOOccasionnelle = 1998,
  TotalMOOccasionnelleETP = 182
)

cols <- c(
  "ChefETP",
  "NbCoexploitants", "CoexploitantsETP",
  "NbMOPermFam", "MOPermFamETP",
  "NbMoPermNonFam", "MoPermNonFamETP",
  "NbMOOccasionnelle", "MOOccasionnelleETP"
)

test_that("Les valeurs des ETP sont correctes", {
  aVerifier <- testUnitaireETP |> summarize(across(all_of(cols), ~ round(sum(.x, na.rm = TRUE)), .names = "Total{.col}"))
  for (col in cols) {
    expect_equal(aVerifier[[paste0("Total", col)]], expected_values[[paste0("Total", col)]])
  }
})

writeCSVTraites(rga23_etp)

# ETAPE 2 - Ajout de la variable SurfacesProdVegetales (hors cocoteraies et hors pâturages) dans la table prodVegetales 

## Récupération des surfaces Hors pâturages et Hors cocoteraies
rga23_surfacesCultures_HC_HP <- readCSV("rga23_surfacesCultures.csv") |>
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
  )) |>
  group_by(interview__key) |>
  summarize(SurfacesTotalesClassiques = sum(replace_na(SurfaceCult, 0)))

## Récupération de la surface de Jardins océaniens, Ajout de la surface obtenue précédemment et Save
rga23_prodVegetales <- left_join(readCSV("rga23_prodVegetales.csv"), rga23_surfacesCultures_HC_HP) |>
  mutate(SurfaceProdVegetales_HC_HP = replace_na(SurfacesTotalesClassiques,0) + replace_na(SurfaceJardins,0)) |>
  select(-SurfacesTotalesClassiques)

test_that("Test de la somme des surfaces de production végétales", {
  testUnitaireSurface <- left_join(rga23_prodVegetales, rga23_general) |> filter(indicRGA23 == 1)
  result <- testUnitaireSurface |> summarize(round(sum(SurfaceProdVegetales_HC_HP) / 10000)) |> pull()
  expect_equal(result, 3135)
})

writeCSVTraites(rga23_prodVegetales)

# Etape 3 - Regroupement des fichiers plats pour faciliter le travail dans Excel

## Récupération de tous les fichiers CSV sauvegardés
file_list <- list.files(path = Sys.getenv("cheminAcces"), pattern = "^rga23_", full.names = TRUE)
for (file in file_list) {
  file_name <- tools::file_path_sans_ext(basename(file))
  assign(file_name, read_csv2(file))
}

## Regroupement des 9 fichiers plats (y compris variables supplémentaires)
rga23_tousFichiersPlats <- left_join(rga23_general, rga23_exploitations) |>
  left_join(rga23_coprahculteurs) |>
  left_join(rga23_mainOeuvre) |>
  left_join(rga23_etp) |>
  left_join(rga23_tape) |>
  left_join(rga23_prodVegetales) |>
  left_join(rga23_prodAnimales) |>
  left_join(rga23_peche)

writeCSVTraites(rga23_tousFichiersPlats)
