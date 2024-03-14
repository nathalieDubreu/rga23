# Critères éleveurs
eleveursChampRGA <- readCSV("rga23_prodAnimales.csv") |>
  filter(
    (
      # 10	• 2 bovins de plus de 2 ans (y compris vaches)
      replace_na(NbAutresBovinsLait, 0) +
        replace_na(NbAutresBovinsViande, 0) +
        replace_na(NbGenissesLait, 0) +
        replace_na(NbGenissesViande, 0) +
        replace_na(NbTaureauxLait, 0) +
        replace_na(NbTaureauxViande, 0) +
        replace_na(NbVachesLait, 0) +
        replace_na(NbVachesViande, 0) >= 2) |
      (
        # 11	• 6 brebis ou chèvres
        replace_na(NbBrebis, 0) +
          replace_na(NbChevres, 0) >= 6) |
      (
        # 12	• 1 truie mère
        replace_na(NbTruiesMaternite, 0) +
          replace_na(NbTruiesGestVides, 0) >= 1) |
      (
        # 13	• 10 lapines mères
        replace_na(NbLapinesMeres, 0) >= 10) |
      (
        # 14	• 200 poulets
        replace_na(NbPouletsChairCoqs, 0) >= 200) |
      (
        # 15	• 100 poules pondeuses
        replace_na(NombrePoules0, 0) +
          replace_na(NombrePoules1, 0) +
          replace_na(NombrePoules3, 0) >= 100) |
      (
        # 16	• 30 ruches
        replace_na(NbRuchesPourProduire, 0) +
          replace_na(NbRuchettes, 0) >= 30) |
      (
        # 17	• 4 naissances d'équidés
        replace_na(NbNaissEquides, 0) >= 4)
  )

# Critères cultivateurs + cas des jardins océaniens au sens de permaculture avec plus de 1000m² de maraichage

cultivateursChampRGA1 <- readCSV("rga23_prodVegetales.csv") |>
  mutate(nbTypesCultures = CultPresentesJardins__10 +
           CultPresentesJardins__20 +
           CultPresentesJardins__30 +
           CultPresentesJardins__40 +
           CultPresentesJardins__50 +
           CultPresentesJardins__70 +
           CultPresentesJardins__80) |>
  filter(
    # 1	Superficie agricole utilisée	10000	m²
    SurfaceTotalProdAgri >= 10000 |
      # 2	Terres arables	10000	m²
      totalSurfDeclarees >= 10000 |
      # 6	Jardins océaniens	3000	m²
      SurfaceJardins >= 3000 |
      # Permaculture de plus de 1000m² de maraichage (critère 8)
      (SurfaceJardins >= 1000 & nbTypesCultures == 1 & CultPresentesJardins__10 == 1) 
  )

culturesChampRGA <- readInputCSV("culturesChampRGA.csv") |> select(culture_id, idSeuilRGA)
rga23_surfacesCultures <- readCSV("rga23_surfacesCultures.csv")

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
idExploitantsDansLeChamp <- full_join(
  eleveursChampRGA |> select(interview__key) |> mutate(ElevageValideRGA = 1),
  idCultivateursChampRGA |> mutate(CultureValideRGA = 1),
  by = c("interview__key")
)

rm(cultivateursChampRGA1, cultivateursChampRGA2, culturesChampRGA, idCultivateursChampRGA, eleveursChampRGA)