### ETP des chefs d'exploitation

rga23_mainOeuvreETP <- rga23_mainOeuvre |>
  mutate(ChefETP = case_when(
    (TpsTravailChefExpl == 1) ~ 0.25,
    (TpsTravailChefExpl == 2) ~ 0.5,
    (TpsTravailChefExpl == 3) ~ 0.75,
    (TpsTravailChefExpl == 4) ~ 1,
    (is.na(TpsTravailChefExpl)) ~ 1
  ))

etpChef <- rga23_mainOeuvreETP |>
  summarize(
    nbChefsEnEtp = sum(ChefETP),
    nbChefs = n()
  )

etpChefArchipel <- rga23_mainOeuvreETP |>
  group_by(Archipel_1) |>
  summarize(
    nbChefs = n(),
    nbChefsEnEtp = sum(ChefETP)
  )

writeCSV(etpChef)
writeCSV(etpChefArchipel)

## Coexploitants

rga23_coexploitantsETP <- rga23_coexploitants |>
  mutate(CoexpoitantsETP = case_when(
    (TempsTravailCoExploit == 1) ~ 0.25,
    (TempsTravailCoExploit == 2) ~ 0.5,
    (TempsTravailCoExploit == 3) ~ 0.75,
    (TempsTravailCoExploit == 4) ~ 1,
    (is.na(TempsTravailCoExploit)) ~ 1
  ))

etpCoexploitants <- rga23_coexploitantsETP |>
  summarize(
    nbCoexploitants = n(),
    nbCoexploitantsEnEtp = sum(CoexpoitantsETP)
  )

etpCoexploitantsArchipel <- rga23_coexploitantsETP |>
  group_by(Archipel_1) |>
  summarize(
    nbCoexploitants = n(),
    nbCoexploitantsEnEtp = sum(CoexpoitantsETP)
  )

writeCSV(etpCoexploitants)
writeCSV(etpCoexploitantsArchipel)

## Main d'oeuvre

### Permanente familiale

rga23_moPermanenteFamETP <- rga23_moPermanenteFam |>
  mutate(MOPermFamETP = case_when(
    (TempsTravailMOFamPerm == 1) ~ 0.25,
    (TempsTravailMOFamPerm == 2) ~ 0.5,
    (TempsTravailMOFamPerm == 3) ~ 0.75,
    (TempsTravailMOFamPerm == 4) ~ 1,
    (is.na(TempsTravailMOFamPerm)) ~ 1
  ))

etpMOPermFam <- rga23_moPermanenteFamETP |>
  summarize(
    nbMOPermFam = n(),
    nbMOPermFamEnEtp = sum(MOPermFamETP)
  )

etpMOPermFamArchipel <- rga23_moPermanenteFamETP |>
  group_by(Archipel_1) |>
  summarize(
    nbMOPermFam = n(),
    nbMOPermFamEnEtp = sum(MOPermFamETP)
  )

writeCSV(etpMOPermFam)
writeCSV(etpMOPermFamArchipel)

### Permanente non familiale

rga23_mainOeuvreETP <- rga23_mainOeuvreETP |>
  mutate(
    MoPermNonFamETP_femmes =
      ifelse(!is.na(nbFemMONonFamPerm__1), nbFemMONonFamPerm__1 * 0.25, 0) +
        ifelse(!is.na(nbFemMONonFamPerm__2), nbFemMONonFamPerm__2 * 0.5, 0) +
        ifelse(!is.na(nbFemMONonFamPerm__3), nbFemMONonFamPerm__3 * 0.75, 0) +
        ifelse(!is.na(nbFemMONonFamPerm__4), nbFemMONonFamPerm__4, 0),
    MoPermNonFamETP_hommes =
      ifelse(!is.na(nbHomMONonFamPerm__1), nbHomMONonFamPerm__1 * 0.25, 0) +
        ifelse(!is.na(nbHomMONonFamPerm__2), nbHomMONonFamPerm__2 * 0.5, 0) +
        ifelse(!is.na(nbHomMONonFamPerm__3), nbHomMONonFamPerm__3 * 0.75, 0) +
        ifelse(!is.na(nbHomMONonFamPerm__4), nbHomMONonFamPerm__4, 0),
    MoPermNonFamETP = MoPermNonFamETP_femmes + MoPermNonFamETP_hommes
  )

etpMOPermNonFam <- rga23_mainOeuvreETP |>
  summarize(
    nbMOPermFam = sum(nbFemmesNFPerm, na.rm = TRUE) + sum(nbHommesNFPerm, na.rm = TRUE),
    nbMOPermFamEnEtp = sum(MoPermNonFamETP)
  )

etpMOPermNonFamArchipel <- rga23_mainOeuvreETP |>
  group_by(Archipel_1) |>
  summarize(
    nbMOPermFam = sum(nbFemmesNFPerm, na.rm = TRUE) + sum(nbHommesNFPerm, na.rm = TRUE),
    nbMOPermFamEnEtp = sum(MoPermNonFamETP)
  )

writeCSV(etpMOPermNonFam)
writeCSV(etpMOPermNonFamArchipel)


### Occasionnelle

rga23_mainOeuvreETP <- rga23_mainOeuvreETP |>
  mutate(totalTempsTravailHOccas = case_when(
    UniteDureeMOOccas == 2 ~ 8 * DureeMOOccas,
    UniteDureeMOOccas == 3 ~ 8 * 5 * DureeMOOccas,
    UniteDureeMOOccas == 4 ~ 8 * 5 * 52/12 * DureeMOOccas,
    TRUE ~ DureeMOOccas
  ))

etpMOOccas <- rga23_mainOeuvreETP |>
  summarize(
    nbMOMAOccas = sum(totalMOOccas, na.rm = TRUE),
    nbMOOccasEnEtp = sum(totalTempsTravailHOccas, na.rm = TRUE) / 1600
  )

etpMOOccasArchipel <- rga23_mainOeuvreETP |>
  group_by(Archipel_1) |>
  summarize(
    nbMOMAOccas = sum(totalMOOccas, na.rm = TRUE),
    nbMOOccasEnEtp = sum(totalTempsTravailHOccas, na.rm = TRUE) / 1600
  )

writeCSV(etpMOOccas)
writeCSV(etpMOOccasArchipel)
