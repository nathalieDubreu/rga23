rga23 <- readCSV("rga23.csv")

# Points coté élevages
eleveursPointsCAPL <- rga23 |>
  mutate(
    nombrePointsElevages = 20 * ifelse(is.na(NbRuchesPourProduire), 0, NbRuchesPourProduire) +
      20 * ifelse(is.na(NbRuchettes), 0, NbRuchettes) +
      35 * ifelse(is.na(NbJeunesEngrLait), 0, NbJeunesEngrLait) +
      35 * ifelse(is.na(NbJeunesEngrViande), 0, NbJeunesEngrViande) +
      35 * ifelse(is.na(NbTaureauxLait), 0, NbTaureauxLait) +
      35 * ifelse(is.na(NbTaureauxViande), 0, NbTaureauxViande) +
      100 * ifelse(is.na(NbVachesLait), 0, NbVachesLait) +
      40 * ifelse(is.na(NbVachesViande), 0, NbVachesViande) +
      35 * ifelse(is.na(NbBoucs), 0, NbBoucs) +
      ifelse(ChevresLait == 1, 50 * NbChevres, 10 * NbChevres) +
      35 * ifelse(is.na(EquidesJeunesBat), 0, EquidesJeunesBat) +
      35 * ifelse(is.na(EquidesJeunesSportsLoisirs), 0, EquidesJeunesSportsLoisirs) +
      100 * ifelse(is.na(EtalonsBat), 0, EtalonsBat) +
      100 * ifelse(is.na(EtalonsSportsLoisirs), 0, EtalonsSportsLoisirs) +
      50 * ifelse(is.na(HongresBat), 0, HongresBat) +
      50 * ifelse(is.na(HongresSportsLoisirs), 0, HongresSportsLoisirs) +
      100 * ifelse(is.na(JumentsPonettesBat), 0, JumentsPonettesBat) +
      100 * ifelse(is.na(JumentsPonettesSportsLoisirs), 0, JumentsPonettesSportsLoisirs) +
      20 * ifelse(is.na(NbLapinesMeres), 0, NbLapinesMeres) +
      5 * ifelse(is.na(NbLapinsSevresEngrais), 0, NbLapinsSevresEngrais) +
      35 * ifelse(is.na(NbBeliers), 0, NbBeliers) +
      10 * ifelse(is.na(NbBrebis), 0, NbBrebis) +
      35 * ifelse(is.na(NbPorcsEngraissement), 0, NbPorcsEngraissement) +
      100 * ifelse(is.na(NbTruiesGestVides), 0, NbTruiesGestVides) +
      100 * ifelse(is.na(NbTruiesMaternite), 0, NbTruiesMaternite) +
      35 * ifelse(is.na(NbVerrats), 0, NbVerrats) +
      1 * ifelse(is.na(NbAutresVolailles), 0, NbAutresVolailles) +
      1 * ifelse(is.na(NbCailles), 0, NbCailles) +
      3 * ifelse(is.na(NbCanards), 0, NbCanards) +
      1 * ifelse(is.na(NbDindesDindons), 0, NbDindesDindons) +
      1 * ifelse(is.na(NbOies), 0, NbOies) +
      1 * ifelse(is.na(NbPintades), 0, NbPintades) +
      3 * ifelse(is.na(NbPouletsChairCoqs), 0, NbPouletsChairCoqs) +
      2 * ifelse(is.na(NombrePoules0), 0, NombrePoules0) +
      2 * ifelse(is.na(NombrePoules1), 0, NombrePoules1) +
      2 * ifelse(is.na(NombrePoules3), 0, NombrePoules3)
  )

# Points coté cultures
culturesChampCAPL <- readInputCSV("culturesChampCAPL.csv") |>
  rename(culture_id = idProdRGA) |>
  select(culture_id, AutreConditionNecessaire, PointsParUnite)
rga23_surfacesCultures <- readCSV("rga23_surfacesCultures.csv")

cultivateursPointsCAPL <- left_join(rga23_surfacesCultures, culturesChampCAPL, by = c("culture_id")) |>
  mutate(PointsCaplBase = SurfaceCult * ifelse(is.na(PointsParUnite), 0,PointsParUnite),
         PointsCaplSuppIrrigation = ifelse((AutreConditionNecessaire == "Irrigue*2" & !is.na(SurfaceIrrig)), SurfaceIrrig * PointsParUnite, 0),
         PointsCapl = PointsCaplBase + ifelse(is.na(PointsCaplSuppIrrigation), 0, PointsCaplSuppIrrigation)) |>
  group_by(interview__key) |>
  summarize(nombrePointsCultures = sum(PointsCapl, na.rm = TRUE))

full_join(cultivateursPointsCAPL, cultivateursPointsCAPL, by = c("interview__key")) |> filter(nombrePointsCultures.x != nombrePointsCultures.y)

# Ensemble des exploitants avec les points elevages et cultures
idExploitantsPointsCAPL <- full_join(
  eleveursPointsCAPL |> select(interview__key, nombrePointsElevages),
  cultivateursPointsCAPL |> select(interview__key, nombrePointsCultures),
  by = c("interview__key")
) |> mutate(PointsCAPL = ifelse(is.na(nombrePointsElevages), 0, nombrePointsElevages) + ifelse(is.na(nombrePointsCultures), 0, nombrePointsCultures))

# idExploitantsPointsCAPL |> filter(nombrePointsElevages >= 300) |> count()
# idExploitantsPointsCAPL |> filter(nombrePointsCultures >= 300) |> count()
idExploitantsPointsCAPL |> filter(PointsCAPL >= 300) |> count()

rm(eleveursPointsCAPL, cultivateursPointsCAPL, culturesChampCAPL, rga23_surfacesCultures)
