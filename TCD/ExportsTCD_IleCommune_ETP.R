library(testthat)

# TCD 11 : Effectifs et ETP

rga23_mainOeuvreETP <- inner_join(rga23_champ_Ile_Commune,
                                  readCSV("rga23_mainOeuvre.csv") |> select(-Ile, -Commune),
                                  by = "interview__key"
) |> mutate(
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

## ETP des coexploitants

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

## ETP MO Permanente familiale

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


# Regroupement pour export

TCD11 <- left_join(rga23_mainOeuvreETP,
                   rga23_coexploitantsETP,
                   by = "interview__key"
) |>
  left_join(rga23_moPermanenteFamETP,
            by = "interview__key"
  ) |>
  select(
    interview__key,
    Archipel_1,
    Ile,
    Commune,
    ChefETP,
    NbCoexploitants,
    CoexploitantsETP,
    NbMOPermFam,
    MOPermFamETP,
    NbMoPermNonFam,
    MoPermNonFamETP,
    NbMOOccasionnelle,
    MOOccasionnelleETP
  ) |>
  mutate(across(where(is.numeric), ~ coalesce(., 0)))
writeCSV(TCD11)

# TEST 

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

cols <- c("ChefETP", 
          "NbCoexploitants", "CoexploitantsETP", 
          "NbMOPermFam","MOPermFamETP", 
          "NbMoPermNonFam", "MoPermNonFamETP", 
          "NbMOOccasionnelle","MOOccasionnelleETP")

test_that("Les valeurs résumées sont correctes", {
  aVerifier <- TCD11 |> summarize(across(all_of(cols), ~ round(sum(.x, na.rm = TRUE)), .names = "Total{.col}"))
  
  for (col in cols) {
    expect_equal(aVerifier[[paste0("Total", col)]], expected_values[[paste0("Total", col)]])
  }
})
