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

eleveursBio <- rga23_exploitations |>
  filter(RaisonsRecensement__2 == 1 & eligibilite == 1) |>
  mutate(`Exploitations avec élevages` = case_when(
    AgriBio == 1 ~ "Totalement bio",
    AgriBio == 2 ~ "Pas bio",
    AgriBio == 3 ~ "En partie bio"
  )) |>
  group_by(`Exploitations avec élevages`) |>
  summarise(n = n()) |>
  mutate(`En %` = round((n / sum(n) * 100), 2)) |>
  select(!n)