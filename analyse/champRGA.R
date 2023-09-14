source("analyse/eligibles.R")

# Critères éleveurs
eleveursChampRGA <- eligiblesRGA |>
  filter(
    (
      # 10	• 2 bovins de plus de 2 ans (y compris vaches)
      ifelse(is.na(NbAutresBovinsLait), 0, NbAutresBovinsLait) +
        ifelse(is.na(NbAutresBovinsViande), 0, NbAutresBovinsViande) +
        ifelse(is.na(NbGenissesLait), 0, NbGenissesLait) +
        ifelse(is.na(NbGenissesViande), 0, NbGenissesViande) +
        ifelse(is.na(NbTaureauxLait), 0, NbTaureauxLait) +
        ifelse(is.na(NbTaureauxViande), 0, NbTaureauxViande) +
        ifelse(is.na(NbVachesLait), 0, NbVachesLait) +
        ifelse(is.na(NbVachesViande), 0, NbVachesViande) >= 2) |
      (
        # 11	• 6 brebis ou chèvres
        ifelse(is.na(NbBrebis), 0, NbBrebis) +
          ifelse(is.na(NbChevres), 0, NbChevres) >= 6) |
      (
        # 12	• 1 truie mère
        ifelse(is.na(NbTruiesMaternite), 0, NbTruiesMaternite) +
          ifelse(is.na(NbTruiesGestVides), 0, NbTruiesGestVides) >= 1) |
      (
        # 13	• 10 lapines mères
        ifelse(is.na(NbLapinesMeres), 0, NbLapinesMeres) >= 10) |
      (
        # 14	• 200 poulets
        ifelse(is.na(NbPouletsChairCoqs), 0, NbPouletsChairCoqs) >= 200) |
      (
        # 15	• 100 poules pondeuses
        ifelse(is.na(NombrePoules0), 0, NombrePoules0) +
          ifelse(is.na(NombrePoules1), 0, NombrePoules1) +
          ifelse(is.na(NombrePoules3), 0, NombrePoules3) >= 100) |
      (
        # 16	• 30 ruches
        ifelse(is.na(NbRuchesPourProduire), 0, NbRuchesPourProduire) +
          ifelse(is.na(NbRuchettes), 0, NbRuchettes) >= 30) |
      (
        # 17	• 4 naissances d'équidés
        ifelse(is.na(NbNaissEquides), 0, NbNaissEquides) >= 4)
  )

# Critères cultivateurs

cultivateursChampRGA1 <- eligiblesRGA |>
  filter(
    # 1	Superficie agricole utilisée	10000	m²
    SurfaceTotalProdAgri >= 10000 |
      # 2	Terres arables	10000	m²
      totalSurfDeclarees >= 10000 |
      # 6	Jardins océaniens	3000	m²
      SurfaceJardins >= 3000
  )

culturesChampRGA <- readCSV("culturesChampRGA.csv") |> select(culture_id, idSeuilRGA)

cultivateursChampRGA2 <- left_join(rga23_surfacesCultures, culturesChampRGA, by = c("culture_id")) |>
  group_by(interview__key, idSeuilRGA) |>
  summarize(SurfaceCulturesSeuil = sum(SurfaceCult)) |>
  filter(
    (
      # 3	Pomme de terre et ensemble des racines et tubercules	3000	m²
      idSeuilRGA == 3 & SurfaceCulturesSeuil >= 3000) |
      (
        # 4	Canne à sucre	3000	m²
        idSeuilRGA == 4 & SurfaceCulturesSeuil >= 3000) |
      (
        # 5	Cultures fruitières (y compris bananes et ananas)	3000	m²
        idSeuilRGA == 5 & SurfaceCulturesSeuil >= 3000) |
      (
        # 7	Ensemble PPAM, cultures ornementales et pépinières	3000	m²
        idSeuilRGA == 7 & SurfaceCulturesSeuil >= 3000) |
      (
        # 8	Légumes frais et fraises	1000	m²
        idSeuilRGA == 8 & SurfaceCulturesSeuil >= 1000) |
      (
        # 9	Serres et abris hauts	100	m²
        idSeuilRGA == 9 & SurfaceCulturesSeuil >= 100)
  )

idCultivateursChampRGA <- rbind(cultivateursChampRGA1 |> select(interview__key), cultivateursChampRGA2 |> select(interview__key) |> distinct()) |>
  distinct()

# Ensemble des exploitants dans le champ
idExploitantsDansLeChamp <- rbind(
  eleveursChampRGA |> select(interview__key), idCultivateursChampRGA
) |> distinct()
