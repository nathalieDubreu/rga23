## Champ : 4080 exploitations au sens du RGA 2023

## Récupération des variables Ile et Commune
rga23_champ_Ile_Commune <- left_join(readCSV("rga23_general.csv") |> filter(indicRGA23 == 1),
  readCSV("rga23_exploitations.csv") |> select(interview__key, eligibilite, IleExploitation, CommuneExploitation),
  by = "interview__key"
) |>
  left_join(readCSV("rga23_mainOeuvre.csv") |> select(interview__key, Ile, Commune),
    by = "interview__key"
  ) |>
  left_join(
    readCSV("rga23_coprahculteurs.csv") |> select(interview__key, eligibiliteCoprah),
    by = "interview__key"
  ) |>
  mutate(
    Cultivateurs = ifelse(indicRGA23 == 1 & RaisonsRecensement__1 == 1 & eligibilite == 1, 1, 0),
    Eleveurs = ifelse(indicRGA23 == 1 & RaisonsRecensement__2 == 1 & eligibilite == 1, 1, 0),
    ProducteursCoprah = ifelse(indicRGA23 == 1 & RaisonsRecensement__3 == 1 & eligibiliteCoprah == 1, 1, 0),
    Ile = case_when(
      !is.na(IleExploitation) ~ IleExploitation,
      TRUE ~ Ile
    ),
    Commune = case_when(
      !is.na(CommuneExploitation) ~ CommuneExploitation,
      TRUE ~ Commune
    )
  ) |>
  select(-IleExploitation, -CommuneExploitation, -starts_with("indic"), -starts_with("eligibilite"), -statut_collecte, -starts_with("RaisonsRecensement"), -PointsCAPL, -ActiviteEnquete)

## TCD 1 : nombres d'exploitants (cultures et/ou élevage et/ou coprah)
TCD1 <- rga23_champ_Ile_Commune
writeCSV(TCD1)

## TCD 2 : caractéristiques des chefs d'exploitations (hommes/femmes, âge)
TCD2 <- left_join(rga23_champ_Ile_Commune,
  readCSV("rga23_mainOeuvre.csv") |> mutate(
    age = 2023 - as.numeric(substring(DateNaissChefExpl, 7, 10)),
    homme = case_when(SexeChefExpl == 1 ~ 0, SexeChefExpl == 2 ~ 1),
    femme = case_when(SexeChefExpl == 1 ~ 1, SexeChefExpl == 2 ~ 0)
  ) |>
    select(interview__key, homme, femme, age),
  by = "interview__key"
)
writeCSV(TCD2)

## TCD 3 : SAU totale déclarée
TCD3 <- inner_join(rga23_champ_Ile_Commune,
  readCSV("rga23_prodVegetales.csv") |> select(interview__key, SurfaceTotalProdAgri),
  by = "interview__key"
)
writeCSV(TCD3)

## TCD 4 : détail des surfaces végétales dont jachères par type
recalculSurfacesType <- inner_join(
  rga23_champ_Ile_Commune,
  readCSV("rga23_surfacesCultures.csv"),
  by = "interview__key"
) |>
  filter(culture_id != 701 & culture_id != 702 & culture_id != 705 & culture_id != 307 & culture_id != 308 & culture_id != 309) |>
  group_by(interview__key, TypeCulture) |>
  summarize(SurfacesTotalesClassiques = sum(ifelse(is.na(SurfaceCult), 0, SurfaceCult))) |>
  mutate(TypeCulture = case_when(
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
  spread(key = TypeCulture, value = SurfacesTotalesClassiques, fill = 0)

TCD4 <- inner_join(rga23_champ_Ile_Commune,
  readCSV("rga23_prodVegetales.csv") |> select(interview__key, SurfaceJardins),
  by = "interview__key"
) |>
  left_join(recalculSurfacesType,
    by = "interview__key"
  ) |>
  mutate(across(everything(), ~ ifelse(is.na(.), 0, .)))
writeCSV(TCD4)

# TCD 5 : détail des surfaces végétales classiques (y compris des pâturages)
TCD5 <- inner_join(
  rga23_champ_Ile_Commune,
  readCSV("rga23_surfacesCultures.csv"),
  by = "interview__key"
) |>
  filter(culture_id != 307 & culture_id != 308 & culture_id != 309) |>
  mutate(TypeCulture = case_when(
    (TypeCulture == 10) ~ "10 - Cultures maraîchères",
    (TypeCulture == 20) ~ "20 - Cultures vivrières",
    (TypeCulture == 30) ~ "30 - Cultures fruitières (hors cocoteraies)",
    (TypeCulture == 40) ~ "40 - Feuillages et cultures florales (hors pépinières)",
    (TypeCulture == 50) ~ "50 - Plantes aromatiques, stimulantes et médicinales",
    (TypeCulture == 60) ~ "60 - Pépinières (plantes vendues en pot)",
    (TypeCulture == 70) ~ "70 - Cultures fourragères (Y COMPRIS pâturages)",
    (TypeCulture == 80) ~ "80 - Jachères",
    TRUE ~ as.character(TypeCulture)
  )) |>
  left_join(readInputCSV("cultures.csv"),
    by = "culture_id"
  ) |>
  mutate(culture = paste0(libelleCulture, " (", culture_id, ")")) |>
  select(interview__key, Archipel_1, Ile, Commune, TypeCulture, culture_id, culture, SurfaceCult)
writeCSV(TCD5)

# TCD 6 : nombre d'éleveurs et effectifs par espèces
TCD6 <- inner_join(
  rga23_champ_Ile_Commune,
  readCSV("rga23_prodAnimales.csv"),
  by = "interview__key"
) |>
  mutate(
    EleveurBovins = PresenceAnimaux__1,
    EleveurOvins = PresenceAnimaux__2,
    EleveurPorcins = PresenceAnimaux__3,
    EleveurVolailles = PresenceAnimaux__4,
    EleveurPoulesPondeuses = ifelse(PresenceAnimaux__4 == 1 & (TypeVolailles__1 == 1 | TypeVolailles__3 == 1 | TypeVolailles__4 == 1), 1, 0),
    EleveurEquides = PresenceAnimaux__5,
    EleveurLapins = PresenceAnimaux__6,
    Apiculteurs = PresenceAnimaux__7,
    EleveurCaprins = PresenceAnimaux__8,
    NombreBovins = replace_na(nbTotalBovins, 0),
    NombreOvins = replace_na(nbTotalOvins, 0),
    NombrePorcins = replace_na(nbTotalPorcs, 0),
    NombreVolailles = rowSums(across(
      c("NbOies", "NbCanards", "NbCailles", "NbPintades", "NbPouletsChairCoqs", "NbPoulettes", "NbPoussins", "NombrePoules0", "NombrePoules1", "NombrePoules3"),
      ~ (coalesce(., 0))
    )),
    NombrePoulesPondeuses = rowSums(across(
      c("NombrePoules0", "NombrePoules1", "NombrePoules3"),
      ~ (coalesce(., 0))
    )),
    NombreEquides = replace_na(nbTotalEquides, 0),
    NombreLapins = rowSums(across(
      c("NbLapereaux", "NbLapinesFutures", "NbLapinesMeres", "NbLapinsReprod", "NbLapinsSevresEngrais"),
      ~ (coalesce(., 0))
    )),
    NombreRuchesPourProduire = replace_na(NbRuchesPourProduire, 0),
    NombreCaprins = replace_na(nbTotalCaprins, 0)
  ) |>
  select(
    interview__key,
    Archipel_1,
    Ile,
    Commune,
    EleveurBovins,
    EleveurOvins,
    EleveurPorcins,
    EleveurVolailles,
    EleveurPoulesPondeuses,
    EleveurEquides,
    EleveurLapins,
    Apiculteurs,
    EleveurCaprins,
    NombreBovins,
    NombreOvins,
    NombrePorcins,
    NombreVolailles,
    NombrePoulesPondeuses,
    NombreEquides,
    NombreLapins,
    NombreRuchesPourProduire,
    NombreCaprins
  )
writeCSV(TCD6)

# TCD 7 : Zoom sur les éleveurs Bovins
TCD7 <- inner_join(
  rga23_champ_Ile_Commune,
  readCSV("rga23_prodAnimales.csv"),
  by = "interview__key"
) |>
  filter(PresenceAnimaux__1 == 1) |>
  select(
    interview__key,
    Archipel_1,
    Ile,
    Commune,
    nbTotalBovins,
    NbAutresBovinsLait,
    NbAutresBovinsViande,
    NbBovinsAttache,
    NbBovinsLiberte,
    NbBovinsPaturage,
    NbGenissesLait,
    NbGenissesViande,
    NbJeunesEngrLait,
    NbJeunesEngrViande,
    nbTotalBovins,
    nbTotalBovinsLait,
    nbTotalBovinsViande,
    NbTaureauxLait,
    NbTaureauxViande,
    NbVachesLait,
    NbVachesViande,
    NbVeauxLait,
    NbVeauxViande
  ) |>
  mutate(across(where(is.numeric), ~ coalesce(., 0)))
writeCSV(TCD7)

# TCD 8 : Production d'oeufs
TCD8 <- inner_join(
  rga23_champ_Ile_Commune,
  readCSV("rga23_prodAnimales.csv"),
  by = "interview__key"
) |>
  filter(PresenceAnimaux__4 == 1 & (TypeVolailles__1 == 1 | TypeVolailles__3 == 1 | TypeVolailles__4 == 1)) |>
  mutate(
    PoulesEnCage = replace_na(NombrePoules3, 0),
    NbOeufsPoulesEnCage = replace_na(ProductionPoules3, 0),
    AutresPoules = replace_na(NombrePoules0, 0) + replace_na(NombrePoules1, 0),
    NbOeufsAutresPoules = replace_na(ProductionPoules0, 0) + replace_na(ProductionPoules1, 0)
  ) |>
  select(
    interview__key,
    Archipel_1,
    Ile,
    Commune,
    PoulesEnCage,
    NbOeufsPoulesEnCage,
    AutresPoules,
    NbOeufsAutresPoules
  )
writeCSV(TCD8)

# TCD 9 : Production de miel
TCD9 <- inner_join(
  rga23_champ_Ile_Commune,
  readCSV("rga23_prodAnimales.csv"),
  by = "interview__key"
) |>
  filter(PresenceAnimaux__7 == 1) |>
  select(
    interview__key,
    Archipel_1,
    Ile,
    Commune,
    NbRuchettes,
    NbRuchesPourProduire,
    NbRuchesRecoltees,
    ProductionRuches,
    ProductionExporteeRuches
  ) |>
  mutate(across(where(is.numeric), ~ coalesce(., 0)))
writeCSV(TCD9)
