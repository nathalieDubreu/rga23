# Demande ISPF  25/04/2024

# Poules pondeuses d'œufs de consommation (en cage - code 3)....................1/1
# Poules pondeuses d'œufs de consommation (plein air ou au sol - code 1 ou 2)...3/3
# Poules pondeuses d'œufs de consommation (biologique - code 0).................4/4
# Poulets de chair et coqs .....................................................7/7

rga23_poulesPoulets <- inner_join(
  readCSV("rga23_prodAnimales.csv") |>
    filter(TypeVolailles__1 == 1 | TypeVolailles__3 == 1 | TypeVolailles__4 == 1 | TypeVolailles__7 == 1) |>
    mutate(
      PresencePoulesPondeuses = case_when(
        TypeVolailles__1 == 1 | TypeVolailles__3 == 1 | TypeVolailles__4 == 1 ~ 1,
        TRUE ~ 0
      ),
      NombrePoulesPondeuses = replace_na(NombrePoules0, 0) + replace_na(NombrePoules1, 0) + replace_na(NombrePoules3, 0),
      PresencePouletsChairCoqs = case_when(
        TypeVolailles__7 == 1 ~ 1,
        TRUE ~ 0
      ),
      NombrePouletsChairCoqs = replace_na(NbPouletsChairCoqs, 0),
      TotalPoulesPondeusesPouletsChairCoqs = NombrePoulesPondeuses + NombrePouletsChairCoqs
    ),
  readCSV("rga23_general.csv") |> filter(indicRGA23 == 1)
)

# Nombre total d'élevages (ayant poulets de chair ou poules pondeuses) par subdi
# - Nombre d'élevages de poulets de chair par subdi
# - Nombre d'élevages de poules pondeuses par subdi
# (certains peuvent avoir les deux)
nbElevages <- rga23_poulesPoulets |>
  group_by(PresencePoulesPondeuses, PresencePouletsChairCoqs) |>
  count()
writeCSV(nbElevages)
nbElevagesArchipel <- rga23_poulesPoulets |>
  group_by(Archipel_1, PresencePoulesPondeuses, PresencePouletsChairCoqs) |>
  count()
writeCSV(nbElevagesArchipel)

# - Nombre d'élevages (ayant poulets de chair ou poules pondeuses) par taille (<500 ;   [500 à 1000[ ;   [1000 à 4000[ ;   4000 et + poulets)
nbElevagesParTaille <- rga23_poulesPoulets |>
  mutate(Taille = case_when(
    TotalPoulesPondeusesPouletsChairCoqs < 500 ~ "<500",
    TotalPoulesPondeusesPouletsChairCoqs < 1000 ~ "[500 à 1000[",
    TotalPoulesPondeusesPouletsChairCoqs < 4000 ~ "[1000 à 4000[",
    TRUE ~ "4000 et +"
  )) |>
  group_by(Taille) |>
  count()
writeCSV(nbElevagesParTaille)

# - Nombre total d'élevages (ayant poulets de chair ou poules pondeuses) par genre (homme / femme) du chef d'exploitation
nbElevagesGenre <- inner_join(
  rga23_poulesPoulets,
  readCSV("rga23_mainOeuvre.csv")
) |>
  group_by(SexeChefExpl) |>
  count()
writeCSV(nbElevagesGenre)

# - Nombre total d'élevages (ayant poulets de chair ou poules pondeuses) par age du chef d'exploitation ( < 26 ans ;   [26 à 55 ans [   ;   [55 ans à 65 ans[  ;   65 ans et + )
nbElevagesClasseAge <- inner_join(
  rga23_poulesPoulets,
  readCSV("rga23_mainOeuvre.csv")
) |>
  mutate(
    age = 2023 - as.numeric(substring(DateNaissChefExpl, 7, 10)),
    `Chefs d'exploitation par classe d'age` = case_when(
      age < 26 ~ "1 - < 26 ans",
      age < 55 ~ "2 - [26 à 55 ans [",
      age < 65 ~ "3 - [55 ans à 65 ans[",
      age >= 65 ~ "3 - 65 ans et +",
      TRUE ~ "Non réponse"
    )
  ) |>
  group_by(`Chefs d'exploitation par classe d'age`) |>
  count()
writeCSV(nbElevagesClasseAge)

# - SAU des élevages (ayant poulets de chair ou poules pondeuses) selon le mode de faire-valoir des terres.
## INDISPO

# - Emploi total en ETP des élevages (ayant poulets de chair ou poules pondeuses)

rga23_mainOeuvre <- inner_join(
  rga23_poulesPoulets |> select(interview__key),
  readCSV("rga23_mainOeuvre.csv"),
  by = "interview__key"
)

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
    enEtp = sum(ChefETP),
    nombre = n()
  )

## Coexploitants

rga23_coexploitants <- inner_join(
  rga23_poulesPoulets |> select(interview__key),
  readCSV("rga23_coexploitants.csv"),
  by = "interview__key"
)

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
    nombre = n(),
    enEtp = sum(CoexpoitantsETP)
  )

## Main d'oeuvre

### Permanente familiale

rga23_moPermanenteFamETP <- inner_join(readCSV("rga23_moPermanenteFam.csv"),
  rga23_poulesPoulets |> select(interview__key),
  by = "interview__key"
) |>
  mutate(MOPermFamETP = case_when(
    (TempsTravailMOFamPerm == 1) ~ 0.25,
    (TempsTravailMOFamPerm == 2) ~ 0.5,
    (TempsTravailMOFamPerm == 3) ~ 0.75,
    (TempsTravailMOFamPerm == 4) ~ 1,
    (is.na(TempsTravailMOFamPerm)) ~ 1
  ))

etpMOPermFam <- rga23_moPermanenteFamETP |>
  summarize(
    nombre = n(),
    enEtp = sum(MOPermFamETP)
  )

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
    nombre = sum(nbFemmesNFPerm, na.rm = TRUE) + sum(nbHommesNFPerm, na.rm = TRUE),
    enEtp = sum(MoPermNonFamETP)
  )

### Occasionnelle

rga23_mainOeuvreETP <- rga23_mainOeuvreETP |>
  mutate(totalTempsTravailHOccas = case_when(
    UniteDureeMOOccas == 2 ~ 8 * DureeMOOccas,
    UniteDureeMOOccas == 3 ~ 8 * 5 * DureeMOOccas,
    UniteDureeMOOccas == 4 ~ 8 * 5 * 52 / 12 * DureeMOOccas,
    TRUE ~ DureeMOOccas
  ))

etpMOOccas <- rga23_mainOeuvreETP |>
  summarize(
    nombre = sum(totalMOOccas, na.rm = TRUE),
    enEtp = sum(totalTempsTravailHOccas, na.rm = TRUE) / 1600
  )

nombreEtETP <- rbind(
  etpChef |> mutate(Statut = "Chefs d'exploitations"),
  etpCoexploitants |> mutate(Statut = "Coexploitants"),
  etpMOPermFam |> mutate(Statut = "Main d'oeuvre permanente familiale"),
  etpMOPermNonFam |> mutate(Statut = "Main d'oeuvre permanente non familiale"),
  etpMOOccas |> mutate(Statut = "Main d'oeuvre occasionnelle")
) |>
  select(Statut, nombre, enEtp)
writeCSV(nombreEtETP)
