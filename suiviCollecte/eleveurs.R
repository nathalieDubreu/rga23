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
eligiblesEleveurs |>
  filter((PresenceAnimaux__1 == 1 | PresenceAnimaux__2 == 1 | PresenceAnimaux__5 == 1 | PresenceAnimaux__8 == 1) &
    RaisonsRecensement__1 == 0) |>
  filter(interview__key != "88-95-70-40" &
    interview__key != "46-85-34-03" &
    interview__key != "56-35-32-39" &
    interview__key != "50-81-86-04" &
    interview__key != "64-94-31-21" &
    interview__key != "59-39-54-16" &
    interview__key != "32-78-62-54" &
    interview__key != "75-51-90-78" &
    interview__key != "12-76-02-08" &
    interview__key != "66-16-40-58") |>
  select(interview__key, interview__status, id_enqueteur_ech, PresenceAnimaux__1, PresenceAnimaux__2, PresenceAnimaux__5, PresenceAnimaux__8)

eleveursVolailles <- eligiblesEleveurs |>
  filter(PresenceAnimaux__4 == 1)

eleveursPoulesPondeuses <- eligiblesEleveurs |>
  filter(TypeVolailles__1 == 1 | TypeVolailles__3 == 1 | TypeVolailles__4 == 1)

eleveursPoulesPondeuses |>
  rename(
    `Poules en cage - code 3` = TypeVolailles__1,
    `Poules plein air ou au sol - code 1 ou 2` = TypeVolailles__3,
    `Poules bio - code 0` = TypeVolailles__4
  ) |>
  group_by(`Poules en cage - code 3`, `Poules plein air ou au sol - code 1 ou 2`, `Poules bio - code 0`) |>
  count()

test <- eleveursPoulesPondeuses |> select(interview__key, NombrePoules3, ProductionPoules3, NombrePoules1, ProductionPoules1, NombrePoules0, ProductionPoules0)

eleveursPoulesPondeuses |> summarize(
  NbPoules3 = sum(NombrePoules3, na.rm = TRUE),
  NbOeufs3 = sum(ProductionPoules3, na.rm = TRUE),
  OeufsParPoules3 = round(NbOeufs3 / NbPoules3, 0),
  NbMinOeufsPoules3 = round(min(ProductionPoules3 / NombrePoules3, na.rm = TRUE), 0),
  NbMaxOeufsPoules3 = round(max(ProductionPoules3 / NombrePoules3, na.rm = TRUE), 0)
)

aVerifier <- eleveursPoulesPondeuses |>
  mutate(OeufsParPoules3 = ProductionPoules3 / NombrePoules3) |>
  select(interview__key, interview__status, OeufsParPoules3, ProductionPoules3, NombrePoules3)

eleveursPoulesPondeuses |> summarize(
  NbPoules12 = sum(NombrePoules1, na.rm = TRUE),
  NbOeufs12 = sum(ProductionPoules1, na.rm = TRUE),
  OeufsParPoules12 = round(NbOeufs12 / NbPoules12, 0),
  NbMinOeufsPoules12 = round(min(ProductionPoules1 / NombrePoules1, na.rm = TRUE), 0),
  NbMaxOeufsPoules12 = round(max(ProductionPoules1 / NombrePoules1, na.rm = TRUE), 0)
)

aVerifier <- eleveursPoulesPondeuses |>
  mutate(OeufsParPoules1 = ProductionPoules1 / NombrePoules1) |>
  select(interview__key, interview__status, OeufsParPoules1, ProductionPoules1, NombrePoules1)

eleveursPoulesPondeuses |> summarize(
  NbPoules0 = sum(NombrePoules0, na.rm = TRUE),
  NbOeufs0 = sum(ProductionPoules0, na.rm = TRUE),
  OeufsParPoules0 = round(NbOeufs0 / NbPoules0, 0),
  NbMinOeufsPoules0 = round(min(ProductionPoules0 / NombrePoules0, na.rm = TRUE), 0),
  NbMaxOeufsPoules0 = round(max(ProductionPoules0 / NombrePoules0, na.rm = TRUE), 0)
)

aVerifier <- eleveursPoulesPondeuses |>
  mutate(OeufsParPoules0 = ProductionPoules0 / NombrePoules0) |>
  select(interview__key, interview__status, OeufsParPoules0, ProductionPoules0, NombrePoules0)

## Poules cages sans fientes (ni fraiches ni séchées) -> A rejeter si plus de 100 poules
aVerifier <- eligiblesEleveurs |>
  filter(TypeVolailles__1 == 1 & TypeEngraisOrga__3 == 0 & TypeEngraisOrga__7 == 0) |>
  select(interview__key, interview__status, id_enqueteur_ech, NombrePoules3)

apiculteurs <- eligiblesEleveurs |>
  filter(PresenceAnimaux__7 == 1) |>
  select(interview__key, interview__status, NbRuchesPourProduire, NbRuchesRecoltees, ProductionRuches) |>
  mutate(KiloMiel = ProductionRuches / NbRuchesRecoltees)

apiculteurs |> summarize(
  nbRuchesPourProduire = sum(NbRuchesPourProduire),
  nbRuchesRecoltees = sum(NbRuchesRecoltees, na.rm = TRUE),
  moyennekiloMiel = sum(ProductionRuches, na.rm = TRUE) / sum(NbRuchesRecoltees, na.rm = TRUE),
  maxKiloMiel = max(KiloMiel, na.rm = TRUE),
  minKiloMiel = min(KiloMiel, na.rm = TRUE)
)

## Porcs et caprins ni en extérieur ni en intérieur
eligiblesEleveurs |>
  filter((AccesBatimentPorcins == 2 & AccesParcoursPorcins == 2) | (CaprinsPleinAir == 2 & AccesBatimentCaprins == 2)) |>
  select(interview__key, interview__status, id_enqueteur_ech)

## Porcs avec bâtiments mais pas de lisier coché (Rejetter si nb de porcs > 15 ou les plus récents)
aVerifier <- eligiblesEleveurs |>
  filter(AccesBatimentPorcins == 1 & TypeEngraisOrga__1 == 0) |>
  select(interview__key, interview__status, id_enqueteur_ech, nbTotalPorcs, AutreTypeEngraisOrga)

## Autre type d'engrais
aVerifier <- eligiblesEleveurs |>
  select(interview__key, interview__status, id_enqueteur_ech, AutreTypeEngraisOrga) |>
  filter(!is.na(AutreTypeEngraisOrga))

rm(eleveursPoulesPondeuses, eleveursVolailles, apiculteurs, test)
