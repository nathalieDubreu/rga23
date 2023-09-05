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
    RaisonsRecensement__1 == 0)

eleveurs <- readCSV("rga23_prodAnimales.csv")
eleveursVolailles <- eleveurs |>
  filter(PresenceAnimaux__4 == 1)

eleveursPoulesPondeuses <- eleveurs |>
  filter(TypeVolailles__1 == 1 | TypeVolailles__3 == 1 | TypeVolailles__4 == 1)

eleveursPoulesPondeuses |>
  group_by(TypeVolailles__1, TypeVolailles__3, TypeVolailles__4) |>
  count()

# eleveursPoulesPondeuses |> select(interview__key, NombrePoules3, ProductionPoules3, NombrePoules1, ProductionPoules1)

eleveursPoulesPondeuses |> summarize(
  NbMoyenPoules3 = sum(NombrePoules3, na.rm = TRUE) / sum(TypeVolailles__1, na.rm = TRUE),
  NbOeufsMoyenPoules3 = sum(ProductionPoules3, na.rm = TRUE) / sum(TypeVolailles__1, na.rm = TRUE),
  OeufsParPoules3 = NbOeufsMoyenPoules3 / NbMoyenPoules3,
  NbMoyenPoules12 = sum(NombrePoules1, na.rm = TRUE) / sum(TypeVolailles__3, na.rm = TRUE),
  NbOeufsMoyenPoules12 = sum(ProductionPoules1, na.rm = TRUE) / sum(TypeVolailles__3, na.rm = TRUE),
  OeufsParPoules12 = NbOeufsMoyenPoules12 / NbMoyenPoules12
)

eleveursPoulesPondeuses |> summarize(
  NbMoyenPoules0 = sum(NombrePoules0, na.rm = TRUE) / sum(TypeVolailles__4, na.rm = TRUE),
  NbOeufsMoyenPoules0 = sum(ProductionPoules0, na.rm = TRUE) / sum(TypeVolailles__4, na.rm = TRUE),
  OeufsParPoules0 = NbOeufsMoyenPoules0 / NbMoyenPoules0
)