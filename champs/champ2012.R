rga23 <- readCSV("rga23.csv")

# Critères éleveurs
eleveursChamp2012 <- rga23 |>
  filter(
    (
      #  • Autres volailles : plus de 50 animaux de plus de 30 jours.
      ifelse(is.na(NbAutresVolailles), 0, NbAutresVolailles) +
        ifelse(is.na(NbCailles), 0, NbCailles) +
        ifelse(is.na(NbCanards), 0, NbCanards) +
        ifelse(is.na(NbDindesDindons), 0, NbDindesDindons) +
        ifelse(is.na(NbOies), 0, NbOies) +
        ifelse(is.na(NbPintades), 0, NbPintades) +
        ifelse(is.na(NbPoulettes), 0, NbPoulettes) > 10) |
      (
        # • Bovins : plus de 10 animaux de plus de 30 jours.
        ifelse(is.na(NbAutresBovinsLait), 0, NbAutresBovinsLait) +
          ifelse(is.na(NbAutresBovinsViande), 0, NbAutresBovinsViande) +
          ifelse(is.na(NbGenissesLait), 0, NbGenissesLait) +
          ifelse(is.na(NbGenissesViande), 0, NbGenissesViande) +
          ifelse(is.na(NbTaureauxLait), 0, NbTaureauxLait) +
          ifelse(is.na(NbTaureauxViande), 0, NbTaureauxViande) +
          ifelse(is.na(NbVachesLait), 0, NbVachesLait) +
          ifelse(is.na(NbVachesViande), 0, NbVachesViande) +
          ifelse(is.na(NbJeunesEngrLait), 0, NbJeunesEngrLait) +
          ifelse(is.na(NbJeunesEngrViande), 0, NbJeunesEngrViande) > 10) |
      (
        # • Caprins : plus de 10 animaux de plus de 30 jours.
        ifelse(is.na(NbChevres), 0, NbChevres) +
          ifelse(is.na(NbChevrettes), 0, NbChevrettes) +
          ifelse(is.na(NbCabris), 0, NbCabris) +
          ifelse(is.na(NbBoucs), 0, NbBoucs) >= 10) |
      (
        # • Porcins : plus de 10 animaux de plus de 30Kg.
        ifelse(is.na(nbTotalPorcs), 0, nbTotalPorcs) -
          ifelse(is.na(NbPorceletsNonSevres), 0, NbPorceletsNonSevres) > 10) |
      (
        # • Ovins : plus de 10 animaux de plus de 30 jours.
        ifelse(is.na(NbBrebis), 0, NbBrebis) +
          ifelse(is.na(NbBeliers), 0, NbBeliers) +
          ifelse(is.na(NbAgnelles), 0, NbAgnelles) > 10) |
      (
        # • Lapins : plus de 20 lapins de plus de 30 jours.
        ifelse(is.na(NbLapinesFutures), 0, NbLapinesFutures) +
          ifelse(is.na(NbLapinesMeres), 0, NbLapinesMeres) +
          ifelse(is.na(NbLapinsReprod), 0, NbLapinsReprod) +
          ifelse(is.na(NbLapinsSevresEngrais), 0, NbLapinsSevresEngrais) > 20) |
      (
        # • Poulets de chair : plus de 100 animaux de plus de 30 jours.
        ifelse(is.na(NbPouletsChairCoqs), 0, NbPouletsChairCoqs) > 100) |
      (
        # • Poules pondeuses : plus de 100 animaux de plus de 30 jours.
        ifelse(is.na(NombrePoules0), 0, NombrePoules0) +
          ifelse(is.na(NombrePoules1), 0, NombrePoules1) +
          ifelse(is.na(NombrePoules3), 0, NombrePoules3) > 100) |
      (
        # • Ruches : plus de 20 ruches.
        ifelse(is.na(NbRuchesPourProduire), 0, NbRuchesPourProduire) +
          ifelse(is.na(NbRuchettes), 0, NbRuchettes) > 20) |
      (
        # Equins : plus de 10 animaux de plus de 30 jours.
        ifelse(is.na(TotalEquidesSportLoisirs), 0, TotalEquidesSportLoisirs) +
          ifelse(is.na(TotalEquidesBat), 0, TotalEquidesBat) +
          ifelse(is.na(TotalAnes), 0, TotalAnes) > 10)
  )

# Critères cultivateurs

cultivateursChamp2012_1 <- rga23 |>
  filter(
    # Superficie agricole utilisée > 0,1 ha
    SurfaceTotalProdAgri >= 1000 |
      # Culture intensive >=  0,05 ha  Pépinière
      totalSurfacePepinieres >= 500
  )

culturesChamp2012 <- readCSV("culturesChamp2012.csv") |> select(culture_id, idSeuilRGA)
rga23_surfacesCultures <- readCSV("rga23_surfacesCultures.csv")

cultivateursChamp2012_2 <- left_join(rga23_surfacesCultures, culturesChamp2012, by = c("culture_id")) |>
  group_by(interview__key, idSeuilRGA) |>
  summarize(SurfaceCulturesSeuil = sum(SurfaceCult)) |>
  filter(
    (
      # Horticulture > 500	m²
      idSeuilRGA == 1 & SurfaceCulturesSeuil >= 500) |
      (
        # Vanille > 500	m²
        idSeuilRGA == 2 & SurfaceCulturesSeuil >= 500)
  )

idCultivateursChamp2012 <- rbind(cultivateursChamp2012_1 |> select(interview__key), cultivateursChamp2012_2 |> select(interview__key) |> distinct()) |>
  distinct()

# Ensemble des exploitants dans le champ
idExploitantsDansLeChamp2012 <- full_join(
  eleveursChamp2012 |> select(interview__key) |> mutate(ElevageValide2012 = 1),
  idCultivateursChamp2012 |> mutate(CultureValide2012 = 1),
  by = c("interview__key")
)

rm(cultivateursChamp2012_1, cultivateursChamp2012_2, culturesChamp2012, idCultivateursChamp2012, eleveursChamp2012)
