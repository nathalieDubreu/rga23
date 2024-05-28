## Cheptels d'animaux

eleveursArchipel <- rga23_prodAnimales |>
  group_by(Archipel_1) |>
  summarise(
    EleveursBovins = sum(PresenceAnimaux__1, na.rm = TRUE),
    EleveursOvins = sum(PresenceAnimaux__2, na.rm = TRUE),
    EleveursPorcins = sum(PresenceAnimaux__3, na.rm = TRUE),
    EleveursVolailles = sum(PresenceAnimaux__4, na.rm = TRUE),
    EleveursPoulesPondeuses = sum(TypeVolailles__1 == 1 | TypeVolailles__3 == 1 | TypeVolailles__4 == 1, na.rm = TRUE),
    EleveursEquides = sum(PresenceAnimaux__5, na.rm = TRUE),
    EleveursLapins = sum(PresenceAnimaux__6, na.rm = TRUE),
    EleveursRuches = sum(PresenceAnimaux__7, na.rm = TRUE),
    EleveursCaprins = sum(PresenceAnimaux__8, na.rm = TRUE)
  ) |>
  pivot_longer(cols = starts_with("Eleveurs"), names_to = "Animaux", values_to = "Nombre d'éleveurs") |>
  mutate(Animaux = gsub("^Eleveurs", "", Animaux))

nombreAnimauxArchipel <- rga23_prodAnimales |>
  group_by(Archipel_1) |>
  summarise(
    NombreBovins = sum(nbTotalBovins, na.rm = TRUE),
    NombreOvins = sum(nbTotalOvins, na.rm = TRUE),
    NombrePorcins = sum(nbTotalPorcs, na.rm = TRUE),
    NombreVolailles = sum(across(
      c("NbAutresVolailles", "NbDindesDindons", "NbOies", "NbCanards", "NbCailles", "NbPintades", "NbPouletsChairCoqs", "NbPoulettes", "NbPoussins", "NombrePoules0", "NombrePoules1", "NombrePoules3"),
      ~ sum(coalesce(.x, 0))
    )),
    NombrePoulesPondeuses = sum(across(
      c("NombrePoules0", "NombrePoules1", "NombrePoules3"),
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

Partie4_nbEleveursEtAnimaux_Archipel <- merge(eleveursArchipel, nombreAnimauxArchipel, by = c("Animaux", "Archipel_1"))
writeCSV(Partie4_nbEleveursEtAnimaux_Archipel)

## Vérification secret stat

calculPart <- function(nbAnimauxExploitant, typeAnimal) {
  left_join(rga23_prodAnimales |> filter({{ nbAnimauxExploitant }} > 0),
    Partie4_nbEleveursEtAnimaux_Archipel |> filter(Animaux == {{ typeAnimal }}),
    by = "Archipel_1"
  ) |>
    mutate(part = {{ nbAnimauxExploitant }} / `Nombre d'animaux`) |>
    select(interview__key, Archipel_1, Animaux, {{ nbAnimauxExploitant }}, `Nombre d'animaux`, part) |>
    filter(part >= 0.85)
}

rga23_prodAnimales <- rga23_prodAnimales |>
  mutate(
    NombreVolailles = rowSums(across(
      c("NbAutresVolailles", "NbDindesDindons", "NbOies", "NbCanards", "NbCailles", "NbPintades", "NbPouletsChairCoqs", "NbPoulettes", "NbPoussins", "NombrePoules0", "NombrePoules1", "NombrePoules3"),
      ~ coalesce(.x, 0)
    )),
    NombrePoulesPondeuses = rowSums(across(
      c("NombrePoules0", "NombrePoules1", "NombrePoules3"),
      ~ coalesce(.x, 0)
    )),
    NombreLapins = rowSums(across(
      c("NbLapereaux", "NbLapinesFutures", "NbLapinesMeres", "NbLapinsReprod", "NbLapinsSevresEngrais"),
      ~ coalesce(.x, 0)
    ))
  )

calculPart(nbTotalBovins, "Bovins")
calculPart(nbTotalCaprins, "Caprins")
calculPart(nbTotalEquides, "Equides")
calculPart(NombreLapins, "Lapins")
calculPart(nbTotalOvins, "Ovins")
calculPart(nbTotalPorcs, "Porcins")
calculPart(NbRuchesPourProduire, "Ruches")
calculPart(NombreVolailles, "Volailles")
calculPart(NombrePoulesPondeuses, "PoulesPondeuses")
