library(rlang)

#### Regroupement de modalités de destination des produits
rga23_prodVegetales_regroupements <- rga23_prodVegetales |>
  mutate(
    PartComMaraic__1_4 = PartComMaraic__1 + PartComMaraic__2 + PartComMaraic__3 + PartComMaraic__4,
    PartComMaraic__5_6 = PartComMaraic__5 + PartComMaraic__6,
    PartComMaraic__7_12 = PartComMaraic__7 + PartComMaraic__8 + PartComMaraic__9 + PartComMaraic__10 + PartComMaraic__11 + PartComMaraic__12,
    PartComFruit__1_4 = PartComFruit__1 + PartComFruit__2 + PartComFruit__3 + PartComFruit__4,
    PartComFruit__5_6 = PartComFruit__5 + PartComFruit__6,
    PartComFruit__7_12 = PartComFruit__7 + PartComFruit__8 + PartComFruit__9 + PartComFruit__10 + PartComFruit__11 + PartComFruit__12,
    PartComVivri__1_4 = PartComVivri__1 + PartComVivri__2 + PartComVivri__3 + PartComVivri__4,
    PartComVivri__5_6 = PartComVivri__5 + PartComVivri__6,
    PartComVivri__7_12 = PartComVivri__7 + PartComVivri__8 + PartComVivri__9 + PartComVivri__10 + PartComVivri__11 + PartComVivri__12
  )

calculPartsDestinationByTailleExpl <- function(partComVar, destinationVar, libelleDestination, surfaceConcernee, seuilMin) {
  result <- rga23_prodVegetales_regroupements |>
    filter(!is.na({{ partComVar }})) |>
    mutate(
      {{ destinationVar }} := case_when(
        {{ partComVar }} == 0 ~ paste("0%", !!libelleDestination),
        {{ partComVar }} <= 25 ~ paste("1 à 25%", !!libelleDestination),
        {{ partComVar }} <= 50 ~ paste("25 et 50%", !!libelleDestination),
        {{ partComVar }} <= 75 ~ paste("50 et 75%", !!libelleDestination),
        {{ partComVar }} <= 100 ~ paste("Plus de 75%", !!libelleDestination)
      ),
      TailleExploitation = case_when(
        {{ surfaceConcernee }} <= seuilMin ~ paste("Petites exploitations  -  ", quo_text(enquo(destinationVar))),
        {{ surfaceConcernee }} <= 10000 ~ paste("Moyennes exploitations  -  ", quo_text(enquo(destinationVar))),
        TRUE ~ paste("Grandes exploitations  - ", quo_text(enquo(destinationVar))),
      )
    ) |>
    group_by(TailleExploitation, {{ destinationVar }}) |>
    summarise(`Nb exploitants` = n()) |>
    mutate(`En %` = round(`Nb exploitants` / sum(`Nb exploitants`) * 100, 1)) |>
    arrange({{ destinationVar }})

  return(result)
}

autoConsoMaraichaTailleExpl <- calculPartsDestinationByTailleExpl(PartComMaraic__1, Maraichage, "AutoConsommation", totalSurfaceMarai, 1000)
writeCSV(autoConsoMaraichaTailleExpl)

autoConsoFruitTailleExpl <- calculPartsDestinationByTailleExpl(PartComFruit__1, Fruitier, "AutoConsommation", totalSurfaceFruit, 3000)
writeCSV(autoConsoFruitTailleExpl)

autoConsoVivriTailleExpl <- calculPartsDestinationByTailleExpl(PartComVivri__1, Vivrier, "AutoConsommation", totalSurfaceVivri, 3000)
writeCSV(autoConsoVivriTailleExpl)



modalites_1_4_MaraichaTailleExpl <- calculPartsDestinationByTailleExpl(PartComMaraic__1_4, Maraichage, "Modalites 1 à 4", totalSurfaceMarai, 1000)
writeCSV(modalites_1_4_MaraichaTailleExpl)

modalites_1_4_FruitTailleExpl <- calculPartsDestinationByTailleExpl(PartComFruit__1_4, Fruitier, "Modalites 1 à 4", totalSurfaceFruit, 3000)
writeCSV(modalites_1_4_FruitTailleExpl)

modalites_1_4_VivriTailleExpl <- calculPartsDestinationByTailleExpl(PartComVivri__1_4, Vivrier, "Modalites 1 à 4", totalSurfaceVivri, 3000)
writeCSV(modalites_1_4_VivriTailleExpl)


modalites_5_6_MaraichaTailleExpl <- calculPartsDestinationByTailleExpl(PartComMaraic__5_6, Maraichage, "Modalites 5 et 6", totalSurfaceMarai, 1000)
writeCSV(modalites_5_6_MaraichaTailleExpl)

modalites_5_6_FruitTailleExpl <- calculPartsDestinationByTailleExpl(PartComFruit__5_6, Fruitier, "Modalites 5 et 6", totalSurfaceFruit, 3000)
writeCSV(modalites_5_6_FruitTailleExpl)

modalites_5_6_VivriTailleExpl <- calculPartsDestinationByTailleExpl(PartComVivri__5_6, Vivrier, "Modalites 5 et 6", totalSurfaceVivri, 3000)
writeCSV(modalites_5_6_VivriTailleExpl)


modalites_7_12_MaraichaTailleExpl <- calculPartsDestinationByTailleExpl(PartComMaraic__7_12, Maraichage, "Modalites 7 à 12", totalSurfaceMarai, 1000)
writeCSV(modalites_7_12_MaraichaTailleExpl)

modalites_7_12_FruitTailleExpl <- calculPartsDestinationByTailleExpl(PartComFruit__7_12, Fruitier, "Modalites 7 à 12", totalSurfaceFruit, 3000)
writeCSV(modalites_7_12_FruitTailleExpl)

modalites_7_12_VivriTailleExpl <- calculPartsDestinationByTailleExpl(PartComVivri__7_12, Vivrier, "Modalites 7 à 12", totalSurfaceVivri, 3000)
writeCSV(modalites_7_12_VivriTailleExpl)
