## Cheptels d'animaux

eleveurs <- rga23_prodAnimales |>
  summarise(
    EleveursBovins = sum(PresenceAnimaux__1, na.rm = TRUE),
    EleveursOvins = sum(PresenceAnimaux__2, na.rm = TRUE),
    EleveursPorcins = sum(PresenceAnimaux__3, na.rm = TRUE),
    EleveursVolailles = sum(PresenceAnimaux__4, na.rm = TRUE),
    EleveursEquides = sum(PresenceAnimaux__5, na.rm = TRUE),
    EleveursLapins = sum(PresenceAnimaux__6, na.rm = TRUE),
    EleveursRuches = sum(PresenceAnimaux__7, na.rm = TRUE),
    EleveursCaprins = sum(PresenceAnimaux__8, na.rm = TRUE)
  ) |>
  pivot_longer(cols = starts_with("Eleveurs"), names_to = "Animaux", values_to = "Nombre d'éleveurs") |>
  mutate(Animaux = gsub("^Eleveurs", "", Animaux))

nombreAnimaux <- rga23_prodAnimales |>
  summarise(
    NombreBovins = sum(nbTotalBovins, na.rm = TRUE),
    NombreOvins = sum(nbTotalOvins, na.rm = TRUE),
    NombrePorcins = sum(nbTotalPorcs, na.rm = TRUE),
    NombreVolailles = sum(across(
      c("NbOies", "NbCanards", "NbPintades", "NbPouletsChairCoqs", "NbPoulettes", "NbPoussins", "NombrePoules0", "NombrePoules1", "NombrePoules3"),
      ~ sum(coalesce(.x, 0))
    )),
    NombreEquides = sum(nbTotalEquides, na.rm = TRUE),
    NombreLapins = sum(across(
      c("NbLapereaux", "NbLapinesFutures", "NbLapinesMeres", "NbLapinsReprod", "NbLapinsSevresEngrais"),
      ~ sum(coalesce(.x, 0))
    )),
    NombreRuches = sum(NbRuchesPourProduire, na.rm = TRUE),
    NombreCaprins = sum(nbTotalCaprins, na.rm = TRUE)
  ) |>
  pivot_longer(cols = starts_with("Nombre"), names_to = "Animaux", values_to = "Nombre d'animaux") |>
  mutate(Animaux = gsub("^Nombre", "", Animaux))

cheptels <- merge(eleveurs, nombreAnimaux, by = "Animaux")
writeCSV(cheptels)

nombreEleveursArchipel <- rga23_prodAnimales |>
  group_by(Archipel_1) |>
  summarise(
    EleveursBovins = sum(PresenceAnimaux__1, na.rm = TRUE),
    EleveursOvins = sum(PresenceAnimaux__2, na.rm = TRUE),
    EleveursPorcins = sum(PresenceAnimaux__3, na.rm = TRUE),
    EleveursVolailles = sum(PresenceAnimaux__4, na.rm = TRUE),
    EleveursEquides = sum(PresenceAnimaux__5, na.rm = TRUE),
    EleveursLapins = sum(PresenceAnimaux__6, na.rm = TRUE),
    EleveursRuches = sum(PresenceAnimaux__7, na.rm = TRUE),
    EleveursCaprins = sum(PresenceAnimaux__8, na.rm = TRUE)
  ) |>
  pivot_longer(cols = starts_with("Eleveurs"), names_to = "Animaux", values_to = "Nombre d'éleveurs") |>
  mutate(Animaux = gsub("^Eleveurs", "", Animaux)) |>
  pivot_wider(names_from = Archipel_1, values_from = "Nombre d'éleveurs")
writeCSV(nombreEleveursArchipel)

nombreAnimauxArchipel <- rga23_prodAnimales |>
  group_by(Archipel_1) |>
  summarise(
    NombreBovins = sum(nbTotalBovins, na.rm = TRUE),
    NombreOvins = sum(nbTotalOvins, na.rm = TRUE),
    NombrePorcins = sum(nbTotalPorcs, na.rm = TRUE),
    NombreVolailles = sum(across(
      c("NbOies", "NbCanards", "NbPintades", "NbPouletsChairCoqs", "NbPoulettes", "NbPoussins", "NombrePoules0", "NombrePoules1", "NombrePoules3"),
      ~ sum(coalesce(.x, 0))
    )),
    NombreEquides = sum(nbTotalEquides, na.rm = TRUE),
    NombreLapins = sum(across(
      c("NbLapereaux", "NbLapinesFutures", "NbLapinesMeres", "NbLapinsReprod", "NbLapinsSevresEngrais"),
      ~ sum(coalesce(.x, 0))
    )),
    NombreRuches = sum(NbRuchesPourProduire, na.rm = TRUE),
    NombreCaprins = sum(nbTotalCaprins, na.rm = TRUE)
  ) |>
  pivot_longer(cols = starts_with("Nombre"), names_to = "Animaux", values_to = "Nombre d'animaux") |>
  mutate(Animaux = gsub("^Nombre", "", Animaux)) |>
  pivot_wider(names_from = Archipel_1, values_from = "Nombre d'animaux")
writeCSV(nombreAnimauxArchipel)

nombreEleveursGenre <- left_join(rga23_prodAnimales, rga23_general |> select(interview__key, SexeChefExpl)) |>
  group_by(SexeChefExpl) |>
  summarise(
    EleveursBovins = sum(PresenceAnimaux__1, na.rm = TRUE),
    EleveursOvins = sum(PresenceAnimaux__2, na.rm = TRUE),
    EleveursPorcins = sum(PresenceAnimaux__3, na.rm = TRUE),
    EleveursVolailles = sum(PresenceAnimaux__4, na.rm = TRUE),
    EleveursEquides = sum(PresenceAnimaux__5, na.rm = TRUE),
    EleveursLapins = sum(PresenceAnimaux__6, na.rm = TRUE),
    EleveursRuches = sum(PresenceAnimaux__7, na.rm = TRUE),
    EleveursCaprins = sum(PresenceAnimaux__8, na.rm = TRUE)
  ) |>
  pivot_longer(cols = starts_with("Eleveurs"), names_to = "Animaux", values_to = "Nombre d'éleveurs") |>
  mutate(Animaux = gsub("^Eleveurs", "", Animaux)) |>
  pivot_wider(names_from = SexeChefExpl, values_from = "Nombre d'éleveurs")
writeCSV(nombreEleveursGenre)

nombreAnimauxGenre <- left_join(rga23_prodAnimales, rga23_general |> select(interview__key, SexeChefExpl)) |>
  group_by(SexeChefExpl) |>
  summarise(
    NombreBovins = sum(nbTotalBovins, na.rm = TRUE),
    NombreOvins = sum(nbTotalOvins, na.rm = TRUE),
    NombrePorcins = sum(nbTotalPorcs, na.rm = TRUE),
    NombreVolailles = sum(across(
      c("NbOies", "NbCanards", "NbPintades", "NbPouletsChairCoqs", "NbPoulettes", "NbPoussins", "NombrePoules0", "NombrePoules1", "NombrePoules3"),
      ~ sum(coalesce(.x, 0))
    )),
    NombreEquides = sum(nbTotalEquides, na.rm = TRUE),
    NombreLapins = sum(across(
      c("NbLapereaux", "NbLapinesFutures", "NbLapinesMeres", "NbLapinsReprod", "NbLapinsSevresEngrais"),
      ~ sum(coalesce(.x, 0))
    )),
    NombreRuches = sum(NbRuchesPourProduire, na.rm = TRUE),
    NombreCaprins = sum(nbTotalCaprins, na.rm = TRUE)
  ) |>
  pivot_longer(cols = starts_with("Nombre"), names_to = "Animaux", values_to = "Nombre d'animaux") |>
  mutate(Animaux = gsub("^Nombre", "", Animaux)) |>
  pivot_wider(names_from = SexeChefExpl, values_from = "Nombre d'animaux")
writeCSV(nombreAnimauxGenre)


eleveursBio <- rga23_exploitations |>
  filter(RaisonsRecensement__2 == 1 & eligibilite == 1) |>
  mutate(`Exploitations avec élevages` = case_when(
    AgriBio == 1 ~ "Totalement bio",
    AgriBio == 2 ~ "Pas bio",
    AgriBio == 3 ~ "En partie bio"
  )) |>
  group_by(`Exploitations avec élevages`) |>
  calculPourcentage()

detailsCheptels <- summary <- rga23_prodAnimales |>
  summarise(
    NbAgneaux = sum(NbAgneaux, na.rm = TRUE),
    NbAgnelles = sum(NbAgnelles, na.rm = TRUE),
    NbAutresBovinsLait = sum(NbAutresBovinsLait, na.rm = TRUE),
    NbAutresBovinsViande = sum(NbAutresBovinsViande, na.rm = TRUE),
    NbAutresPorcs = sum(NbAutresPorcs, na.rm = TRUE),
    NbAutresVolailles = sum(NbAutresVolailles, na.rm = TRUE),
    NbBeliers = sum(NbBeliers, na.rm = TRUE),
    NbBoucs = sum(NbBoucs, na.rm = TRUE),
    NbBovinsAttache = sum(NbBovinsAttache, na.rm = TRUE),
    NbBovinsLiberte = sum(NbBovinsLiberte, na.rm = TRUE),
    NbBovinsPaturage = sum(NbBovinsPaturage, na.rm = TRUE),
    NbBrebis = sum(NbBrebis, na.rm = TRUE),
    NbCabris = sum(NbCabris, na.rm = TRUE),
    NbCailles = sum(NbCailles, na.rm = TRUE),
    NbCanards = sum(NbCanards, na.rm = TRUE),
    NbCaprinsAttache = sum(NbCaprinsAttache, na.rm = TRUE),
    NbCaprinsLiberte = sum(NbCaprinsLiberte, na.rm = TRUE),
    NbCaprinsPaturage = sum(NbCaprinsPaturage, na.rm = TRUE),
    NbChevreaux = sum(NbChevreaux, na.rm = TRUE),
    NbChevres = sum(NbChevres, na.rm = TRUE),
    NbChevrettes = sum(NbChevrettes, na.rm = TRUE),
    NbCochettes = sum(NbCochettes, na.rm = TRUE),
    NbDindesDindons = sum(NbDindesDindons, na.rm = TRUE),
    NbEquidesAttache = sum(NbEquidesAttache, na.rm = TRUE),
    NbEquidesLiberte = sum(NbEquidesLiberte, na.rm = TRUE),
    NbEquidesPaturage = sum(NbEquidesPaturage, na.rm = TRUE),
    NbGenissesLait = sum(NbGenissesLait, na.rm = TRUE),
    NbGenissesViande = sum(NbGenissesViande, na.rm = TRUE),
    NbJeunesEngrLait = sum(NbJeunesEngrLait, na.rm = TRUE),
    NbJeunesEngrViande = sum(NbJeunesEngrViande, na.rm = TRUE),
    NbLapereaux = sum(NbLapereaux, na.rm = TRUE),
    NbLapinesFutures = sum(NbLapinesFutures, na.rm = TRUE),
    NbLapinesMeres = sum(NbLapinesMeres, na.rm = TRUE),
    NbLapinsReprod = sum(NbLapinsReprod, na.rm = TRUE),
    NbLapinsSevresEngrais = sum(NbLapinsSevresEngrais, na.rm = TRUE),
    NbNaissEquides = sum(NbNaissEquides, na.rm = TRUE),
    NbOies = sum(NbOies, na.rm = TRUE),
    NbOvinsAttache = sum(NbOvinsAttache, na.rm = TRUE),
    NbOvinsLiberte = sum(NbOvinsLiberte, na.rm = TRUE),
    NbOvinsPaturage = sum(NbOvinsPaturage, na.rm = TRUE),
    NbPintades = sum(NbPintades, na.rm = TRUE),
    NbPorceletsNonSevres = sum(NbPorceletsNonSevres, na.rm = TRUE),
    NbPorceletsPostSevrage = sum(NbPorceletsPostSevrage, na.rm = TRUE),
    NbPorcsEngraissement = sum(NbPorcsEngraissement, na.rm = TRUE),
    NbPouletsChairCoqs = sum(NbPouletsChairCoqs, na.rm = TRUE),
    NbPoulettes = sum(NbPoulettes, na.rm = TRUE),
    NbPoussins = sum(NbPoussins, na.rm = TRUE),
    NbRuchers = sum(NbRuchers, na.rm = TRUE),
    NbTaureauxLait = sum(NbTaureauxLait, na.rm = TRUE),
    NbTaureauxViande = sum(NbTaureauxViande, na.rm = TRUE),
    NbTruiesGestVides = sum(NbTruiesGestVides, na.rm = TRUE),
    NbTruiesMaternite = sum(NbTruiesMaternite, na.rm = TRUE),
    NbVachesLait = sum(NbVachesLait, na.rm = TRUE),
    NbVachesViande = sum(NbVachesViande, na.rm = TRUE),
    NbVeauxLait = sum(NbVeauxLait, na.rm = TRUE),
    NbVeauxViande = sum(NbVeauxViande, na.rm = TRUE),
    NbVerrats = sum(NbVerrats, na.rm = TRUE),
    NombrePoules0 = sum(NombrePoules0, na.rm = TRUE),
    NombrePoules1 = sum(NombrePoules1, na.rm = TRUE),
    NombrePoules3 = sum(NombrePoules3, na.rm = TRUE)
  ) |>
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Somme")
writeCSV(detailsCheptels)

apicultures <- rga23_prodAnimales |>
  filter(PresenceAnimaux__7 == 1) |>
  select(interview__key, NbRuchesPourProduire, NbRuchettes) |>
  mutate(CategorieNombreRuches = case_when(
    NbRuchesPourProduire + NbRuchettes < 10 ~ "0 à 9 : ruches pour produire + ruchettes",
    NbRuchesPourProduire + NbRuchettes < 20 ~ "10 à 29 : ruches pour produire + ruchettes",
    NbRuchesPourProduire + NbRuchettes < 30 ~ "20 à 29 : ruches pour produire + ruchettes",
    NbRuchesPourProduire + NbRuchettes >= 30 ~ "30 et plus : ruches pour produire + ruchettes",
    TRUE ~ "?"
  ))

apicultures |> group_by(CategorieNombreRuches) |>
  count()

