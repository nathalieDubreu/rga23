source("analyse/eligibles.R")

## Eligibles  éleveurs
eligiblesEleveurs <- eligiblesRGA |>
  filter(RaisonsRecensement__2 == 1)

# Bovins........................1/1
# Ovins.........................2/2
# Porcins.......................3/3
# Volailles.....................4/4
# Equidés.......................5/5
# Lapins élevés pour la chair...6/6
# Abeilles......................7/7
# Caprins.......................8/8
eligiblesEleveurs |>
  summarise(
    elevBovins = sum(PresenceAnimaux__1, na.rm = TRUE),
    elevOvins = sum(PresenceAnimaux__2, na.rm = TRUE),
    elevPorcins = sum(PresenceAnimaux__3, na.rm = TRUE),
    elevVolailles = sum(PresenceAnimaux__4, na.rm = TRUE),
    elevEquides = sum(PresenceAnimaux__5, na.rm = TRUE),
    elevLapins = sum(PresenceAnimaux__6, na.rm = TRUE),
    elevAbeilles = sum(PresenceAnimaux__7, na.rm = TRUE),
    elevCaprins = sum(PresenceAnimaux__8, na.rm = TRUE)
  )

## TODO : A vérifier éleveurs sans paturage (pour bovins, caprins, ovins et équidés a minima)
aVerifier <- eligiblesEleveurs |>
  filter((PresenceAnimaux__1 == 1 | PresenceAnimaux__2 == 1 | PresenceAnimaux__5 == 1 | PresenceAnimaux__8 == 1) &
    RaisonsRecensement__1 == 0) |>
  filter(interview__key != "88-95-70-40" & interview__key != "46-85-34-03") |>
  select(interview__key, interview__status, id_enqueteur_ech, PresenceAnimaux__1, PresenceAnimaux__2, PresenceAnimaux__5, PresenceAnimaux__8)

eleveurs <- readCSV("rga23_prodAnimales.csv")
eleveursVolailles <- eleveurs |>
  filter(PresenceAnimaux__4 == 1)

eleveursPoulesPondeuses <- eleveurs |>
  filter(TypeVolailles__1 == 1 | TypeVolailles__3 == 1 | TypeVolailles__4 == 1)

eleveursPoulesPondeuses |>
  rename(
    `Poules en cage - code 3` = TypeVolailles__1,
    `Poules plein air ou au sol - code 1 ou 2` = TypeVolailles__3,
    `Poules bio - code 0` = TypeVolailles__4
  ) |>
  group_by(`Poules en cage - code 3`, `Poules plein air ou au sol - code 1 ou 2`, `Poules bio - code 0`) |>
  count()

# eleveursPoulesPondeuses |> select(interview__key, NombrePoules3, ProductionPoules3, NombrePoules1, ProductionPoules1)

eleveursPoulesPondeuses |> summarize(
  NbMoyenPoules3 = sum(NombrePoules3, na.rm = TRUE) / sum(TypeVolailles__1, na.rm = TRUE),
  NbOeufsMoyenPoules3 = sum(ProductionPoules3, na.rm = TRUE) / sum(TypeVolailles__1, na.rm = TRUE),
  OeufsParPoules3 = NbOeufsMoyenPoules3 / NbMoyenPoules3,
  NbMinOeufsPoules3 = min(ProductionPoules3 / NombrePoules3, na.rm = TRUE),
  NbMaxOeufsPoules3 = max(ProductionPoules3 / NombrePoules3, na.rm = TRUE)
)

# aVerifier <- eleveursPoulesPondeuses |>
#   mutate(OeufsParPoules3 = ProductionPoules3 / NombrePoules3) |>
#   select(interview__key, OeufsParPoules3, ProductionPoules3, NombrePoules3)

eleveursPoulesPondeuses |> summarize(
  NbMoyenPoules12 = sum(NombrePoules1, na.rm = TRUE) / sum(TypeVolailles__3, na.rm = TRUE),
  NbOeufsMoyenPoules12 = sum(ProductionPoules1, na.rm = TRUE) / sum(TypeVolailles__3, na.rm = TRUE),
  OeufsParPoules12 = NbOeufsMoyenPoules12 / NbMoyenPoules12,
  NbMinOeufsPoules12 = min(ProductionPoules1 / NombrePoules1, na.rm = TRUE),
  NbMaxOeufsPoules12 = max(ProductionPoules1 / NombrePoules1, na.rm = TRUE)
)

aVerifier <- eleveursPoulesPondeuses |>
  mutate(OeufsParPoules1 = ProductionPoules1 / NombrePoules1) |>
  select(interview__key, OeufsParPoules1, ProductionPoules1, NombrePoules1)

eleveursPoulesPondeuses |> summarize(
  NbMoyenPoules0 = sum(NombrePoules0, na.rm = TRUE) / sum(TypeVolailles__4, na.rm = TRUE),
  NbOeufsMoyenPoules0 = sum(ProductionPoules0, na.rm = TRUE) / sum(TypeVolailles__4, na.rm = TRUE),
  OeufsParPoules0 = NbOeufsMoyenPoules0 / NbMoyenPoules0,
  NbMinOeufsPoules0 = min(ProductionPoules0 / NombrePoules0, na.rm = TRUE),
  NbMaxOeufsPoules0 = max(ProductionPoules0 / NombrePoules0, na.rm = TRUE)
)

apiculteurs <- eleveurs |>
  filter(PresenceAnimaux__7 == 1) |>
  select(interview__key, NbRuchesPourProduire, NbRuchesRecoltees, ProductionRuches) |>
  mutate(KiloMiel = ProductionRuches / NbRuchesRecoltees)

apiculteurs |> summarize(
  nbRuchesPourProduire = sum(NbRuchesPourProduire),
  nbRuchesRecoltees = sum(NbRuchesRecoltees, na.rm = TRUE),
  moyennekiloMiel = sum(ProductionRuches, na.rm = TRUE) / sum(NbRuchesRecoltees, na.rm = TRUE),
  maxKiloMiel = max(KiloMiel, na.rm = TRUE),
  minKiloMiel = min(KiloMiel, na.rm = TRUE)
)
