# Demande BSE : listing des interview__key des exploitants (>= 400 points) qui déclarent acheter des plants en pépinières publique + cultivent du fruitier

cultivateursFruitiersPlantsPepinierePublique <- inner_join(
  readCSV("rga23_general.csv") |> filter(indicPointsCAPL == 1),
  readCSV("rga23_exploitations.csv") |> filter(ProvenancePlants__2 == 1) |> select(interview__key),
  by = "interview__key"
) |>
  inner_join(readCSV("rga23_prodVegetales.csv") |> filter(CulturesPresentes__30 == 1 | CultPresentesJardins__30 == 1) |> select(interview__key),
    by = "interview__key"
  ) |>
  select(interview__key, Archipel_1)

writeCSV(cultivateursFruitiersPlantsPepinierePublique)

## Pour info : répartition par archipel
cultivateursFruitiersPlantsPepinierePublique |> 
  group_by(Archipel_1) |>
  count()
