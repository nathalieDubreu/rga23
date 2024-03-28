# Demande BSE sur l'Apiculture - 27/03/2024

## Champ : 4080 exploitations au sens du RGA 2023 -> 913 à avoir coché la case Eleveurs

rga23_champ <- readCSV("rga23_general.csv") |>
  filter(indicRGA23 == 1)

rga23_prodAnimales <- inner_join(rga23_champ, readCSV("rga23_prodAnimales.csv"))

comptagesApiculteurs <- rga23_prodAnimales |>
  filter(PresenceAnimaux__7 == 1) |>
  summarise(
    NombreApiculteurs = sum(PresenceAnimaux__7, na.rm = TRUE),
    NombreRuches = sum(NbRuchesPourProduire, na.rm = TRUE),
    NombreRuchesRecoltees = sum(NbRuchesRecoltees, na.rm = TRUE),
    NombreRuchettes = sum(NbRuchettes, na.rm = TRUE),
    ProductionTotaleRuchesEnKilos = sum(ProductionRuches, na.rm = TRUE),
    ProductionMoyenneEnKilos = round(ProductionTotaleRuchesEnKilos / NombreApiculteurs)
  )

rga23_apiculture_comptagesParArchipel <- rga23_prodAnimales |>
  filter(PresenceAnimaux__7 == 1) |>
  group_by(Archipel_1) |>
  summarise(
    NombreApiculteurs = sum(PresenceAnimaux__7, na.rm = TRUE),
    NombreRuches = sum(NbRuchesPourProduire, na.rm = TRUE),
    NombreRuchesRecoltees = sum(NbRuchesRecoltees, na.rm = TRUE),
    NombreRuchettes = sum(NbRuchettes, na.rm = TRUE),
    ProductionTotaleRuchesEnKilos = sum(ProductionRuches, na.rm = TRUE),
    ProductionMoyenneEnKilos = round(ProductionTotaleRuchesEnKilos / NombreRuchesRecoltees)
  ) |> add_row(
    Archipel_1 = "Ensemble de la Polynésie française",
    NombreApiculteurs = comptagesApiculteurs$NombreApiculteurs,
    NombreRuches = comptagesApiculteurs$NombreRuches,
    NombreRuchesRecoltees = comptagesApiculteurs$NombreRuchesRecoltees, 
    NombreRuchettes = comptagesApiculteurs$NombreRuchettes,
    ProductionTotaleRuchesEnKilos = comptagesApiculteurs$ProductionTotaleRuchesEnKilos,
    ProductionMoyenneEnKilos = round(comptagesApiculteurs$ProductionTotaleRuchesEnKilos / comptagesApiculteurs$NombreRuchesRecoltees)
  )
writeCSV(rga23_apiculture_comptagesParArchipel)

rga23_apiculture_comptagesParCategorie <- rga23_prodAnimales |>
  filter(PresenceAnimaux__7 == 1) |>
  mutate(CategorieNombreRuches = case_when(
    NbRuchesPourProduire + NbRuchettes < 30 ~ "0 à 29 ruches (ruches pour produire et ruchettes)",
    NbRuchesPourProduire + NbRuchettes < 50 ~ "30 et 49 ruches (ruches pour produire et ruchettes)",
    NbRuchesPourProduire + NbRuchettes >= 50 ~ "Plus de 50 ruches (ruches pour produire et ruchettes)",
    TRUE ~ "?"
  )) |>
  group_by(CategorieNombreRuches) |>
  summarise(NombreApiculteurs = sum(PresenceAnimaux__7, na.rm = TRUE),
            NombreRuches = sum(NbRuchesPourProduire, na.rm = TRUE),
            NombreRuchesRecoltees = sum(NbRuchesRecoltees, na.rm = TRUE),
            NombreRuchettes = sum(NbRuchettes, na.rm = TRUE),
            ProductionTotaleRuchesEnKilos = sum(ProductionRuches, na.rm = TRUE),
            ProductionMoyenneEnKilos = round(ProductionTotaleRuchesEnKilos / NombreRuchesRecoltees)) |>
  mutate(`Répartition des apiculteurs (en %)` = round(NombreApiculteurs / sum(NombreApiculteurs) * 100, 1)) 
writeCSV(rga23_apiculture_comptagesParCategorie)

