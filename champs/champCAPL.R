# Points coté élevages

calculPointsCAPLElevage <- function(data) {
  dataAvecPoints <- data |>
    mutate(
      nombrePointsElevages =
        20 * replace_na(NbRuchesPourProduire, 0) +
          20 * replace_na(NbRuchettes, 0) +
          35 * replace_na(NbJeunesEngrLait, 0) +
          35 * replace_na(NbJeunesEngrViande, 0) +
          35 * replace_na(NbTaureauxLait, 0) +
          35 * replace_na(NbTaureauxViande, 0) +
          100 * replace_na(NbVachesLait, 0) +
          40 * replace_na(NbVachesViande, 0) +
          35 * replace_na(NbBoucs, 0) +
          ifelse(is.na(ChevresLait), 0, ifelse(ChevresLait == 1, 50 * NbChevres, 10 * NbChevres)) +
          35 * replace_na(EquidesJeunesBat, 0) +
          35 * replace_na(EquidesJeunesSportsLoisirs, 0) +
          100 * replace_na(EtalonsBat, 0) +
          100 * replace_na(EtalonsSportsLoisirs, 0) +
          50 * replace_na(HongresBat, 0) +
          50 * replace_na(HongresSportsLoisirs, 0) +
          100 * replace_na(JumentsPonettesBat, 0) +
          100 * replace_na(JumentsPonettesSportsLoisirs, 0) +
          20 * replace_na(NbLapinesMeres, 0) +
          5 * replace_na(NbLapinsSevresEngrais, 0) +
          35 * replace_na(NbBeliers, 0) +
          10 * replace_na(NbBrebis, 0) +
          35 * replace_na(NbPorcsEngraissement, 0) +
          100 * replace_na(NbTruiesGestVides, 0) +
          100 * replace_na(NbTruiesMaternite, 0) +
          35 * replace_na(NbVerrats, 0) +
          1 * replace_na(NbAutresVolailles, 0) +
          1 * replace_na(NbCailles, 0) +
          3 * replace_na(NbCanards, 0) +
          1 * replace_na(NbDindesDindons, 0) +
          1 * replace_na(NbOies, 0) +
          1 * replace_na(NbPintades, 0) +
          3 * replace_na(NbPouletsChairCoqs, 0) +
          2 * replace_na(NombrePoules0, 0) +
          2 * replace_na(NombrePoules1, 0) +
          2 * replace_na(NombrePoules3, 0)
    )
  return(dataAvecPoints)
}

eleveursPointsCAPL <- calculPointsCAPLElevage(readCSV("rga23_prodAnimales.csv"))

# Points coté cultures
culturesChampCAPL <- readInputCSV("culturesChampCAPL.csv") |>
  rename(culture_id = idProdRGA) |>
  mutate(PointsParUnite = as.numeric(PointsParUnite)) |>
  select(culture_id, AutreConditionNecessaire, PointsParUnite)

cultivateursPointsCAPL <- left_join(readCSV("rga23_surfacesCultures.csv"), culturesChampCAPL, by = c("culture_id")) |>
  mutate(
    PointsCaplBase = SurfaceCult * replace_na(PointsParUnite, 0),
    PointsCaplSuppIrrigation = ifelse((AutreConditionNecessaire == "Irrigue*2" & !is.na(SurfaceIrrig)), SurfaceIrrig * PointsParUnite, 0),
    PointsCapl = PointsCaplBase + replace_na(PointsCaplSuppIrrigation, 0)
  ) |>
  group_by(interview__key) |>
  summarize(nombrePointsCultures = sum(PointsCapl, na.rm = TRUE))

# Points jardins océaniens
## Comme les vergers i.e. 0.1 point par m² si non irrigué et 0.2 si irrigué

calculPointsCAPLJardinsOceaniens <- function(data) {
  dataAvecPoints <- data |>
    mutate(nombrePointsJardinsOceaniens = 0.1 * replace_na(SurfaceJardins, 0)
      + 0.1 * replace_na(SurfaceIrrigJardins, 0))
  return(dataAvecPoints)
}

cultivateursPointsCAPLJardins <- calculPointsCAPLJardinsOceaniens(readCSV("rga23_prodVegetales.csv"))

tousTypesPoints <- full_join(
  eleveursPointsCAPL |> select(interview__key, nombrePointsElevages),
  cultivateursPointsCAPL |> select(interview__key, nombrePointsCultures),
  by = c("interview__key")
) |> full_join(cultivateursPointsCAPLJardins, by = c("interview__key"))

# Ensemble des exploitants avec les points elevages et cultures
idExploitantsPointsCAPL <- left_join(readCSV("rga23_general.csv") |> select(interview__key, indicRGA23_Coprah, Archipel_1),
  tousTypesPoints,
  by = c("interview__key")
) |>
  mutate(
    PointsCAPL = replace_na(nombrePointsElevages, 0) +
      replace_na(nombrePointsCultures, 0) +
      replace_na(nombrePointsJardinsOceaniens, 0)
  ) |>
  select(interview__key, Archipel_1, PointsCAPL, indicRGA23_Coprah)

writeCSV(idExploitantsPointsCAPL)

rm(eleveursPointsCAPL, cultivateursPointsCAPL, culturesChampCAPL)
