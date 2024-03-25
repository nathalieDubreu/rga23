library(rlang)

#### Regroupement de modalités de destination des produits
rga23_prodVegetales_regroupements <- rga23_prodVegetales |>
  mutate(
    ## Regroupements des destinations pour le maraichage
    PartComMaraic__1_4 = rowSums(across(
      all_of(paste0("PartComMaraic__", 1:4)),
      ~ coalesce(., 0)
    )),
    PartComMaraic__5_6 = rowSums(across(
      all_of(paste0("PartComMaraic__", 5:6)),
      ~ coalesce(., 0)
    )),
    PartComMaraic__7_12 = rowSums(across(
      all_of(paste0("PartComMaraic__", 7:12)),
      ~ coalesce(., 0)
    )),
    ## Regroupements des destinations pour le fruitier
    PartComFruit__1_4 = rowSums(across(
      all_of(paste0("PartComFruit__", 1:4)),
      ~ coalesce(., 0)
    )),
    PartComFruit__5_6 = rowSums(across(
      all_of(paste0("PartComFruit__", 5:6)),
      ~ coalesce(., 0)
    )),
    PartComFruit__7_12 = rowSums(across(
      all_of(paste0("PartComFruit__", 7:12)),
      ~ coalesce(., 0)
    )),
    ## Regroupements des destinations pour le vivrier
    PartComVivri__1_4 = rowSums(across(
      all_of(paste0("PartComVivri__", 1:4)),
      ~ coalesce(., 0)
    )),
    PartComVivri__5_6 = rowSums(across(
      all_of(paste0("PartComVivri__", 5:6)),
      ~ coalesce(., 0)
    )),
    PartComVivri__7_12 = rowSums(across(
      all_of(paste0("PartComVivri__", 7:12)),
      ~ coalesce(., 0)
    )),
    ## Regroupements des destinations pour le floral
    PartComFlorale__1_4 = rowSums(across(
      all_of(paste0("PartComFlorale__", 1:4)),
      ~ coalesce(., 0)
    )),
    PartComFlorale__5_6 = rowSums(across(
      all_of(paste0("PartComFlorale__", 5:6)),
      ~ coalesce(., 0)
    )),
    PartComFlorale__7_12 = rowSums(across(
      all_of(paste0("PartComFlorale__", 7:12)),
      ~ coalesce(., 0)
    )),
    ## Regroupements des destinations pour les PPAM
    PartComPlantes__1_4 = rowSums(across(
      all_of(paste0("PartComPlantes__", 1:4)),
      ~ coalesce(., 0)
    )),
    PartComPlantes__5_6 = rowSums(across(
      all_of(paste0("PartComPlantes__", 5:6)),
      ~ coalesce(., 0)
    )),
    PartComPlantes__7_12 = rowSums(across(
      all_of(paste0("PartComPlantes__", 7:12)),
      ~ coalesce(., 0)
    )),
    ## Regroupements des destinations pour les pépinières
    PartComPepinieres__1_4 = rowSums(across(
      all_of(paste0("PartComPepinieres__", 1:4)),
      ~ coalesce(., 0)
    )),
    PartComPepinieres__5_6 = rowSums(across(
      all_of(paste0("PartComPepinieres__", 5:6)),
      ~ coalesce(., 0)
    )),
    PartComPepinieres__7_12 = rowSums(across(
      all_of(paste0("PartComPepinieres__", 7:12)),
      ~ coalesce(., 0)
    ))
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

  write.csv(result, file.path(Sys.getenv("cheminAcces"), "SortiesR", paste("Partie3_", deparse(substitute(destinationVar)), ".csv", sep = "")), row.names = FALSE)

  return(result)
}

## Autoconsommation seule pour certains types de cultures
calculPartsDestinationByTailleExpl(PartComMaraic__1, MaraichageAutoconsommation, totalSurfaceMarai, 1000)
calculPartsDestinationByTailleExpl(PartComFruit__1, FruitierAutoconsommation, totalSurfaceFruit, 3000)
calculPartsDestinationByTailleExpl(PartComVivri__1, VivrierAutoconsommation, totalSurfaceVivri, 3000)

## Destination Hors Vente
DestinationsHV_Maraic_TailleExpl <- calculPartsDestinationByTailleExpl(PartComMaraic__1_4, MaraichageDestinationsHV, totalSurfaceMarai, 1000)
DestinationsHV_Fruit_TailleExpl <- calculPartsDestinationByTailleExpl(PartComFruit__1_4, FruitierDestinationsHV, totalSurfaceFruit, 3000)
DestinationsHV_Vivri_TailleExpl <- calculPartsDestinationByTailleExpl(PartComVivri__1_4, VivrierDestinationsHV, totalSurfaceVivri, 3000)
DestinationsHV_Florale_TailleExpl <- calculPartsDestinationByTailleExpl(PartComFlorale__1_4, FloraleDestinationsHV, totalSurfaceFlorale, 3000)
DestinationsHV_Plantes_TailleExpl <- calculPartsDestinationByTailleExpl(PartComPlantes__1_4, PlantesDestinationsHV, totalSurfacePlantes, 3000)
DestinationsHV_Pepinieres_TailleExpl <- calculPartsDestinationByTailleExpl(PartComPepinieres__1_4, PepinieresDestinationsHV, totalSurfacePepinieres, 3000)

## Vente Directe
VenteDirecte_Maraic_TailleExpl <- calculPartsDestinationByTailleExpl(PartComMaraic__5_6, MaraichageVenteDirecte, totalSurfaceMarai, 1000)
VenteDirecte_Fruit_TailleExpl <- calculPartsDestinationByTailleExpl(PartComFruit__5_6, FruitierVenteDirecte, totalSurfaceFruit, 3000)
VenteDirecte_Vivri_TailleExpl <- calculPartsDestinationByTailleExpl(PartComVivri__5_6, VivrierVenteDirecte, totalSurfaceVivri, 3000)
VenteDirecte_Florale_TailleExpl <- calculPartsDestinationByTailleExpl(PartComFlorale__5_6, FloraleVenteDirecte, totalSurfaceFlorale, 3000)
VenteDirecte_Plantes_TailleExpl <- calculPartsDestinationByTailleExpl(PartComPlantes__5_6, PlantesVenteDirecte, totalSurfacePlantes, 3000)
VenteDirecte_Pepinieres_TailleExpl <- calculPartsDestinationByTailleExpl(PartComPepinieres__5_6, PepinieresVenteDirecte, totalSurfacePepinieres, 3000)

## Vente aux professionnels
VenteAuxProfessionnels_Maraic_TailleExpl <- calculPartsDestinationByTailleExpl(PartComMaraic__7_12, MaraichageVenteAuxProfessionnels, totalSurfaceMarai, 1000)
VenteAuxProfessionnels_Fruit_TailleExpl <- calculPartsDestinationByTailleExpl(PartComFruit__7_12, FruitierVenteAuxProfessionnels, totalSurfaceFruit, 3000)
VenteAuxProfessionnels_Vivri_TailleExpl <- calculPartsDestinationByTailleExpl(PartComVivri__7_12, VivrierVenteAuxProfessionnels, totalSurfaceVivri, 3000)
VenteAuxProfessionnels_Florale_TailleExpl <- calculPartsDestinationByTailleExpl(PartComFlorale__7_12, FloraleVenteAuxProfessionnels, totalSurfaceFlorale, 3000)
VenteAuxProfessionnels_Plantes_TailleExpl <- calculPartsDestinationByTailleExpl(PartComPlantes__7_12, PlantesVenteAuxProfessionnels, totalSurfacePlantes, 3000)
VenteAuxProfessionnels_Pepinieres_TailleExpl <- calculPartsDestinationByTailleExpl(PartComPepinieres__7_12, PepinieresVenteAuxProfessionnels, totalSurfacePepinieres, 3000)


#### Graphiques
library(ggplot2)

ggplot(
  DestinationsHV_Maraic_TailleExpl |> mutate(Proportion = `En %` / 100),
  aes(x = TailleExploitation, y = Proportion, fill = MaraichageDestinationsHV)
) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = scales::percent(Proportion)),
    position = position_stack(vjust = 0.5),
    size = 3, color = "black"
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  labs(
    x = "Taille des exploitations",
    y = "Proportion d'exploitants",
    fill = "Part de destinations hors vente",
    title = "MARAICHAGE - Part de destination hors vente par taille d'exploitation"
  )

nom_fichier <- paste(Sys.getenv("cheminAcces"), "/SortiesR/DestinationsHV_Maraic_TailleExpl.jpg", sep = "")
ggsave(nom_fichier, plot = last_plot(), width = 10, height = 6, units = "in")
