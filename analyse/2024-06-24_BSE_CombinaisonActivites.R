## Champ : 4080 exploitations au sens du RGA 2023

## Restriction au champ
rga23_champ <- left_join(readCSV("rga23_general.csv") |> filter(indicRGA23 == 1),
  readCSV("rga23_exploitations.csv") |> select(interview__key, eligibilite),
  by = "interview__key"
) |>
  left_join(
    readCSV("rga23_coprahculteurs.csv") |> select(interview__key, eligibiliteCoprah),
    by = "interview__key"
  ) |>
  mutate(
    Cultivateurs = ifelse(indicRGA23 == 1 & RaisonsRecensement__1 == 1 & eligibilite == 1, 1, 0),
    Eleveurs = ifelse(indicRGA23 == 1 & RaisonsRecensement__2 == 1 & eligibilite == 1, 1, 0),
    ProducteursCoprah = ifelse(indicRGA23 == 1 & RaisonsRecensement__3 == 1 & eligibiliteCoprah == 1, 1, 0),
  ) |>
  select(-starts_with("indic"), -starts_with("eligibilite"), -statut_collecte, -starts_with("RaisonsRecensement"), -PointsCAPL, -ActiviteEnquete)

## Mise en avant des combinaisons d'activités
combinaisonsActivites <- left_join(
  rga23_champ,
  readCSV("rga23_tousFichiersPlats.csv") |>
    select(
      interview__key,
      starts_with("PresenceAnimaux__"),
      starts_with("CulturesPresentes__"),
      SurfaceJardins,
      ActivitesChefExploit__5
    ),
  by = "interview__key"
) |>
  mutate(
    # Cultures maraîchères....................................10/10
    activite_A = ifelse(replace_na(CulturesPresentes__10, 0) == 1, " Maraichage /", "-/"),
    # Cultures vivrières......................................20 /20
    activite_B = ifelse(replace_na(CulturesPresentes__20, 0) == 1, " Vivrier /", "-/"),
    # Cultures fruitières (hors pépinères) et bois d'oeuvre...30 /30
    activite_C = ifelse(replace_na(CulturesPresentes__30, 0) == 1, " Fruitier /", "-/"),
    # Feuillages et cultures florales (hors pépinières).......40 /40
    activite_D = ifelse(replace_na(CulturesPresentes__40, 0) == 1, " Floral /", "-/"),
    # Plantes aromatiques, stimulantes et médicinales.........50 /50
    activite_E = ifelse(replace_na(CulturesPresentes__50, 0) == 1, " Ppam /", "-/"),
    # Pépinières (plantes vendues en pot).....................60 /60
    activite_F = ifelse(replace_na(CulturesPresentes__60, 0) == 1, " Pepinieres /", "-/"),
    # Cultures fourragères....................................70 /70
    activite_G = ifelse(replace_na(CulturesPresentes__70, 0) == 1, " Fourrageres /", "-/"),
    # Jachères................................................80 /80
    activite_H = ifelse(replace_na(CulturesPresentes__80, 0) == 1, " Jacheres /", "-/"),
    # Jardins océaniens
    activite_I = ifelse(replace_na(SurfaceJardins, 0) > 0, " JardinsOceaniens /", "-/"),
    # Bovins..................................................1 /1
    activite_J = ifelse(replace_na(PresenceAnimaux__1, 0) == 1, " Bovins /", "-/"),
    # Ovins...................................................2 /2
    activite_K = ifelse(replace_na(PresenceAnimaux__2, 0) == 1, " Ovins /", "-/"),
    # Porcins.................................................3 /3
    activite_L = ifelse(replace_na(PresenceAnimaux__3, 0) == 1, " Porcins /", "-/"),
    # Volailles...............................................4 /4
    activite_M = ifelse(replace_na(PresenceAnimaux__4, 0) == 1, " Volailles /", "-/"),
    # Equidés.................................................5 /5
    activite_N = ifelse(replace_na(PresenceAnimaux__5, 0) == 1, " Equides /", "-/"),
    # Lapins élevés pour la chair.............................6 /6
    activite_O = ifelse(replace_na(PresenceAnimaux__6, 0) == 1, " Lapins /", "-/"),
    # Abeilles................................................7 /7
    activite_P = ifelse(replace_na(PresenceAnimaux__7, 0) == 1, " Abeilles /", "-/"),
    # Caprins.................................................8 /8
    activite_Q = ifelse(replace_na(PresenceAnimaux__8, 0) == 1, " Caprins /", "-/"),
    # Peche
    activite_R = ifelse(replace_na(ActivitesChefExploit__5, 0) == 1, " Peche /", "-/"),
    # Coprah
    activite_S = ifelse(replace_na(ProducteursCoprah, 0) == 1, " Coprah", "-"),
    # Concaténation
    Activites = paste0(
      activite_A, activite_B, activite_C, activite_D, activite_E, activite_F,
      activite_G, activite_H, activite_I, activite_J, activite_K, activite_L,
      activite_M, activite_N, activite_O, activite_P, activite_Q, activite_R, activite_S
    )
  ) |>
  select(interview__key, Archipel_1, Activites)

comptagesCombinaisonsActivites <- combinaisonsActivites |>
  group_by(Activites) |>
  summarize(`Nombre d'exploitants` = n()) |>
  arrange(desc(`Nombre d'exploitants`))

writeCSV(comptagesCombinaisonsActivites)

## Zoom sur les combinaisons d'activités qui concernent au moins 25 chefs d'exploitations ou coprahculteurs

comptagesFiltres <- comptagesCombinaisonsActivites |>
  filter(`Nombre d'exploitants` >= 25)

combinaisonsActivitesFiltrees <- combinaisonsActivites |>
  semi_join(comptagesFiltres, by = "Activites") |>
  select(-Archipel_1)

qqStatsParCombinaisonActivite <- rga23_champ |>
  select(interview__key, Archipel_1) |>
  left_join(combinaisonsActivitesFiltrees,
    by = "interview__key"
  ) |>
  mutate(Activites = case_when(
    !is.na(Activites) ~ Activites,
    TRUE ~ "Autres combinaisons"
  )) |>
  left_join(
    readCSV("rga23_mainOeuvre.csv") |> mutate(
      age = 2023 - as.numeric(substring(DateNaissChefExpl, 7, 10)),
      homme = case_when(SexeChefExpl == 1 ~ 0, SexeChefExpl == 2 ~ 1),
      femme = case_when(SexeChefExpl == 1 ~ 1, SexeChefExpl == 2 ~ 0)
    ) |>
      select(interview__key, homme, femme, age),
    by = "interview__key"
  ) |>
  left_join(
    readCSV("rga23_etp.csv"),
    by = "interview__key"
  ) |>
  left_join(
    readCSV("rga23_prodVegetales.csv") |> select(interview__key, SurfaceTotalProdAgri, SurfaceProdVegetales_HC_HP),
    by = "interview__key"
  ) |>
  left_join(
    readCSV("rga23_prodAnimales.csv") |>
      mutate(
        EleveurBovins = PresenceAnimaux__1,
        EleveurOvins = PresenceAnimaux__2,
        EleveurPorcins = PresenceAnimaux__3,
        EleveurVolailles = PresenceAnimaux__4,
        EleveurPoulesPondeuses = ifelse(PresenceAnimaux__4 == 1 & (TypeVolailles__1 == 1 | TypeVolailles__3 == 1 | TypeVolailles__4 == 1), 1, 0),
        EleveurEquides = PresenceAnimaux__5,
        EleveurLapins = PresenceAnimaux__6,
        Apiculteurs = PresenceAnimaux__7,
        EleveurCaprins = PresenceAnimaux__8,
        NombreBovins = replace_na(nbTotalBovins, 0),
        NombreOvins = replace_na(nbTotalOvins, 0),
        NombrePorcins = replace_na(nbTotalPorcs, 0),
        NombreVolailles = rowSums(across(
          c("NbAutresVolailles", "NbDindesDindons", "NbOies", "NbCanards", "NbCailles", "NbPintades", "NbPouletsChairCoqs", "NbPoulettes", "NbPoussins", "NombrePoules0", "NombrePoules1", "NombrePoules3"),
          ~ (coalesce(., 0))
        )),
        NombrePoulesPondeuses = rowSums(across(
          c("NombrePoules0", "NombrePoules1", "NombrePoules3"),
          ~ (coalesce(., 0))
        )),
        NombreEquides = replace_na(nbTotalEquides, 0),
        NombreLapins = rowSums(across(
          c("NbLapereaux", "NbLapinesFutures", "NbLapinesMeres", "NbLapinsReprod", "NbLapinsSevresEngrais"),
          ~ (coalesce(., 0))
        )),
        NombreRuchesPourProduire = replace_na(NbRuchesPourProduire, 0),
        NombreCaprins = replace_na(nbTotalCaprins, 0)
      ) |>
      select(
        interview__key,
        EleveurBovins,
        EleveurOvins,
        EleveurPorcins,
        EleveurVolailles,
        EleveurPoulesPondeuses,
        EleveurEquides,
        EleveurLapins,
        Apiculteurs,
        EleveurCaprins,
        NombreBovins,
        NombreOvins,
        NombrePorcins,
        NombreVolailles,
        NombrePoulesPondeuses,
        NombreEquides,
        NombreLapins,
        NombreRuchesPourProduire,
        NombreCaprins
      ),
    by = "interview__key"
  )
writeCSV(qqStatsParCombinaisonActivite)
