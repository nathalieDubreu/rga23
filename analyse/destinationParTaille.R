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

calculPartsDestinationByTailleExpl <- function(partComVar, destinationVar, surfaceConcernee, seuilMin) {
  result <- rga23_prodVegetales_regroupements |>
    filter(!is.na({{ partComVar }}) & {{ surfaceConcernee }} > 0) |>
    mutate(
      {{ destinationVar }} := case_when(
        {{ partComVar }} <= 25 ~ "0 à 25%",
        {{ partComVar }} <= 50 ~ "25 et 50%",
        {{ partComVar }} <= 75 ~ "50 et 75%",
        {{ partComVar }} <= 100 ~ "Plus de 75%",
        TRUE ~ "???"
      ),
      TailleExploitation = case_when(
        {{ surfaceConcernee }} <= seuilMin ~ "Petites",
        {{ surfaceConcernee }} <= 10000 ~ "Moyennes",
        {{ surfaceConcernee }} > 10000 ~ "Grandes",
        TRUE ~ "???"
      )
    ) |>
    group_by(TailleExploitation, {{ destinationVar }}) |>
    summarise(`Nb exploitants` = n()) |>
    mutate(`En %` = round(`Nb exploitants` / sum(`Nb exploitants`) * 100, 1)) |>
    arrange({{ destinationVar }})

  return(result)
}

autoConsoMaraichaTailleExpl <- calculPartsDestinationByTailleExpl(PartComMaraic__1, MaraichageAutoconsommation, totalSurfaceMarai, 1000)
writeCSV(autoConsoMaraichaTailleExpl)

autoConsoFruitTailleExpl <- calculPartsDestinationByTailleExpl(PartComFruit__1, FruitierAutoconsommation, totalSurfaceFruit, 3000)
writeCSV(autoConsoFruitTailleExpl)

autoConsoVivriTailleExpl <- calculPartsDestinationByTailleExpl(PartComVivri__1, VivrierAutoconsommation, totalSurfaceVivri, 3000)
writeCSV(autoConsoVivriTailleExpl)



DestinationsHV_MaraichaTailleExpl <- calculPartsDestinationByTailleExpl(PartComMaraic__1_4, MaraichageDestinationsHV, totalSurfaceMarai, 1000)
writeCSV(DestinationsHV_MaraichaTailleExpl)

DestinationsHV_FruitTailleExpl <- calculPartsDestinationByTailleExpl(PartComFruit__1_4, FruitierDestinationsHV, totalSurfaceFruit, 3000)
writeCSV(DestinationsHV_FruitTailleExpl)

DestinationsHV_VivriTailleExpl <- calculPartsDestinationByTailleExpl(PartComVivri__1_4, VivrierDestinationsHV, totalSurfaceVivri, 3000)
writeCSV(DestinationsHV_VivriTailleExpl)


VenteDirecte_MaraichaTailleExpl <- calculPartsDestinationByTailleExpl(PartComMaraic__5_6, MaraichageVenteDirecte, totalSurfaceMarai, 1000)
writeCSV(VenteDirecte_MaraichaTailleExpl)

VenteDirecte_FruitTailleExpl <- calculPartsDestinationByTailleExpl(PartComFruit__5_6, FruitierVenteDirecte, totalSurfaceFruit, 3000)
writeCSV(VenteDirecte_FruitTailleExpl)

VenteDirecte_VivriTailleExpl <- calculPartsDestinationByTailleExpl(PartComVivri__5_6, VivrierVenteDirecte, totalSurfaceVivri, 3000)
writeCSV(VenteDirecte_VivriTailleExpl)


VenteAuxProfessionnels_MaraichaTailleExpl <- calculPartsDestinationByTailleExpl(PartComMaraic__7_12, MaraichageVenteAuxProfessionnels, totalSurfaceMarai, 1000)
writeCSV(VenteAuxProfessionnels_MaraichaTailleExpl)

VenteAuxProfessionnels_FruitTailleExpl <- calculPartsDestinationByTailleExpl(PartComFruit__7_12, FruitierVenteAuxProfessionnels, totalSurfaceFruit, 3000)
writeCSV(VenteAuxProfessionnels_FruitTailleExpl)

VenteAuxProfessionnels_VivriTailleExpl <- calculPartsDestinationByTailleExpl(PartComVivri__7_12, VivrierVenteAuxProfessionnels, totalSurfaceVivri, 3000)
writeCSV(VenteAuxProfessionnels_VivriTailleExpl)


#### Graphiques
library(ggplot2)

ggplot(
  DestinationsHV_MaraichaTailleExpl |> mutate(Proportion = `En %` / 100),
  aes(x = TailleExploitation, y = Proportion, fill = MaraichageDestinationsHV)
) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = scales::percent(Proportion)),
    position = position_stack(vjust = 0.5),
    size = 3, color = "black"
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  labs(x = "Taille des exploitations", 
       y = "Proportion d'exploitants", 
       fill = "Part de destinations hors vente", 
       title = "MARAICHAGE - Part de destination hors vente par taille d'exploitation")

nom_fichier <- paste(Sys.getenv("cheminAcces"), "/SortiesR/DestinationsHV_MaraichaTailleExpl.jpg", sep="")
ggsave(nom_fichier, plot = last_plot(), width = 10, height = 6, units = "in")
