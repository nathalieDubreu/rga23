# Critères éleveurs
eleveursChamp2012 <- readCSV("rga23_prodAnimales.csv") |>
  filter(
    (
      #  • Autres volailles : plus de 50 animaux de plus de 30 jours.
      replace_na(NbAutresVolailles, 0) +
        replace_na(NbCailles, 0) +
        replace_na(NbCanards, 0) +
        replace_na(NbDindesDindons, 0) +
        replace_na(NbOies, 0) +
        replace_na(NbPintades, 0) +
        replace_na(NbPoulettes, 0) > 10) |
      (
        # • Bovins : plus de 10 animaux de plus de 30 jours.
        replace_na(NbAutresBovinsLait, 0) +
          replace_na(NbAutresBovinsViande, 0) +
          replace_na(NbGenissesLait, 0) +
          replace_na(NbGenissesViande, 0) +
          replace_na(NbTaureauxLait, 0) +
          replace_na(NbTaureauxViande, 0) +
          replace_na(NbVachesLait, 0) +
          replace_na(NbVachesViande, 0) +
          replace_na(NbJeunesEngrLait, 0) +
          replace_na(NbJeunesEngrViande, 0) > 10) |
      (
        # • Caprins : plus de 10 animaux de plus de 30 jours.
        replace_na(NbChevres, 0) +
          replace_na(NbChevrettes, 0) +
          replace_na(NbCabris, 0) +
          replace_na(NbBoucs, 0) >= 10) |
      (
        # • Porcins : plus de 10 animaux de plus de 30Kg.
        replace_na(nbTotalPorcs, 0) -
          replace_na(NbPorceletsNonSevres, 0) > 10) |
      (
        # • Ovins : plus de 10 animaux de plus de 30 jours.
        replace_na(NbBrebis, 0) +
          replace_na(NbBeliers, 0) +
          replace_na(NbAgnelles, 0) > 10) |
      (
        # • Lapins : plus de 20 lapins de plus de 30 jours.
        replace_na(NbLapinesFutures, 0) +
          replace_na(NbLapinesMeres, 0) +
          replace_na(NbLapinsReprod, 0) +
          replace_na(NbLapinsSevresEngrais, 0) > 20) |
      (
        # • Poulets de chair : plus de 100 animaux de plus de 30 jours.
        replace_na(NbPouletsChairCoqs, 0) > 100) |
      (
        # • Poules pondeuses : plus de 100 animaux de plus de 30 jours.
        replace_na(NombrePoules0, 0) +
          replace_na(NombrePoules1, 0) +
          replace_na(NombrePoules3, 0) > 100) |
      (
        # • Ruches : plus de 20 ruches.
        replace_na(NbRuchesPourProduire, 0) +
          replace_na(NbRuchettes, 0) > 20) |
      (
        # Equins : plus de 10 animaux de plus de 30 jours.
        replace_na(TotalEquidesSportLoisirs, 0) +
          replace_na(TotalEquidesBat, 0) +
          replace_na(TotalAnes, 0) > 10)
  )


# Critères cultivateurs

cultivateursChamp2012_1 <- readCSV("rga23_prodVegetales.csv") |>
  filter(
    # Superficie agricole utilisée > 0,1 ha
    SurfaceTotalProdAgri >= 1000 |
      # Culture intensive >=  0,05 ha  Pépinière
      totalSurfacePepinieres >= 500
  )

culturesChamp2012 <- readInputCSV("culturesChamp2012.csv") |> select(culture_id, idSeuilRGA)
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
