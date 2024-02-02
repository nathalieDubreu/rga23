library(rmarkdown)
library(knitr)
library(tidyr)

source("champs/champRGA.R")

## Ajout d'une indicatrice dans la table RGA pour les exploitants dans le champ (cultures et/ou élevage)
rga23A <- left_join(rga23, idExploitantsDansLeChamp |> mutate(ValideRGA = 1),
  by = c("interview__key")
)

## Ajout d'une indicatrice dans la table RGA pour les coprahculteurs de plus de 2,7 tonnes (identifiants C et X éligibles)
rga23A_valides <- rga23A |>
  mutate(CoprahValideRGA = case_when((eligibiliteCoprah == 1 & substring(id_exploitation, 0, 1) == "C") | (eligibiliteCoprah == 1 & substring(id_exploitation, 0, 1) == "X") ~ 1))

## Restriction au champ 23 du RGA
rga23_champ <- rga23A_valides |>
  filter(ValideRGA == 1 | CoprahValideRGA == 1) |>
  mutate(Archipel_1 = case_when(!is.na(ArchipelExploitation) ~ ArchipelExploitation, TRUE ~ Archipel))

# Tables utiles - restreintes au champ
rga23_parcelles <- inner_join(readCSV("rga23_parcelles.csv"), rga23_champ |> select(interview__key))
rga23_prodVegetales <- inner_join(readCSV("rga23_prodVegetales.csv"), rga23_champ |> select(interview__key))
rga23_prodAnimales <- inner_join(readCSV("rga23_prodAnimales.csv"), rga23_champ |> select(interview__key))
rga23_surfacesCultures <- inner_join(readCSV("rga23_surfacesCultures.csv"), rga23_champ |> select(interview__key))
rga23_coprahculteurs <- inner_join(readCSV("rga23_coprahculteurs.csv"), rga23_champ |> select(interview__key, CoprahValideRGA))
rga23_exploitations <- inner_join(readCSV("rga23_exploitations.csv"), rga23_champ |> select(interview__key, RaisonsRecensement__1, RaisonsRecensement__2))
rga23_general <- inner_join(readCSV("rga23_general.csv"), rga23_champ |> select(interview__key)) |>
  mutate(age = 2023 - as.numeric(substring(DateNaissChefExpl, 7, 10)))
rga23_mainOeuvre <- inner_join(readCSV("rga23_mainOeuvre.csv"), rga23_champ |>
  select(interview__key)) |>
  mutate(totalMAOccas = ifelse(is.na(NbFemOccasAvecLien), 0, NbFemOccasAvecLien) +
    ifelse(is.na(NbFemOccasSansLien), 0, NbFemOccasSansLien) +
    ifelse(is.na(NbHomOccasAvecLien), 0, NbHomOccasAvecLien) +
    ifelse(is.na(NbHomOccasSansLien), 0, NbHomOccasSansLien))

# Profil des exploitations par archipel (cultures, elevage, coprah)
profilExploitationsParArchipel <- rga23_champ %>%
  group_by(Archipel) %>%
  summarize(
    ExploitationsCultures = sum(CultureValideRGA, na.rm = TRUE),
    ExploitationsElevage = sum(ElevageValideRGA, na.rm = TRUE),
    CoprahValideRGA = sum(CoprahValideRGA, na.rm = TRUE)
  )

totalExploitationsCultures <- sum(profilExploitationsParArchipel$ExploitationsCultures, na.rm = TRUE)
totalExploitationsElevage <- sum(profilExploitationsParArchipel$ExploitationsElevage, na.rm = TRUE)
totalCoprahValideRGA <- sum(profilExploitationsParArchipel$CoprahValideRGA, na.rm = TRUE)

profilExploitationsParArchipelEtTotal <- profilExploitationsParArchipel %>%
  add_row(
    Archipel = "Total",
    ExploitationsCultures = totalExploitationsCultures,
    ExploitationsElevage = totalExploitationsElevage,
    CoprahValideRGA = totalCoprahValideRGA
  )

# Surface agricole utilisée par type de faire-valoir (parcelles)

surfacesParFaireValoir <- rga23_parcelles |>
  filter(faireValoirParcelle != -999999999) |>
  mutate(`Faire valoir` = case_when(
    faireValoirParcelle == 1 ~ "1 - Propriétaire du terrain",
    faireValoirParcelle == 2 ~ "2 - Locataire pays",
    faireValoirParcelle == 3 ~ "3 - Autre locataire",
    faireValoirParcelle == 4 ~ "4 - Métayer - gardien",
    faireValoirParcelle == 5 ~ "5 - Usufruitier",
    faireValoirParcelle == 6 ~ "6 - Occupant sans titre (occupation précaire)",
    faireValoirParcelle == 7 ~ "7 - Co-indivisaire (propriétaire en indivision)",
    TRUE ~ as.character(faireValoirParcelle)
  )) |>
  group_by(`Faire valoir`) |>
  summarize(`Surface (en Ha)` = (sum(polygone__area, na.rm = TRUE) + sum(surfaceParcelleNonDelimitee, na.rm = TRUE)) / 10000)

surfacesParcelles <- sum(surfacesParFaireValoir$`Surface (en Ha)`)

surfacesParFaireValoirPercent <- surfacesParFaireValoir |>
  mutate(Pourcentage = round(`Surface (en Ha)` / surfacesParcelles * 100, 1)) |>
  select(`Faire valoir`, Pourcentage)

surfaces <- rga23_prodVegetales |> summarize(
  SAUTotale = round(sum(SurfaceTotalProdAgri, na.rm = TRUE) / 10000),
  SurfaceTotaleDeclaree = round(sum(totalSurfDeclarees, na.rm = TRUE) / 10000)
)

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

## Chefs d'exploitations

genreChef <- rga23_general |>
  mutate(homme = case_when(SexeChefExpl == 1 ~ 0, SexeChefExpl == 2 ~ 1), femme = case_when(SexeChefExpl == 1 ~ 1, SexeChefExpl == 2 ~ 0)) |>
  summarize(
    NbHommes = sum(homme, na.rm = TRUE),
    NbFemmes = sum(femme, na.rm = TRUE),
    TauxFemmes = NbFemmes / NbHommes
  )

### Bio ?

rga23_exploitations |> group_by(AgriBio) |> count()

### Temps de travail du chef d'exploitation

tempsTravailChef <- rga23_mainOeuvre |>
  filter(!is.na(TpsTravailChefExpl)) |>
  mutate(`Temps de travail du chef d'exploitation` = case_when(
    (TpsTravailChefExpl == 1) ~ "1 : Moins de 1/2 temps",
    (TpsTravailChefExpl == 2) ~ "2 : 1/2 temps",
    (TpsTravailChefExpl == 3) ~ "3 : Entre 1/2 temps et temps complet",
    (TpsTravailChefExpl == 4) ~ "4 : Temps complet",
    (is.na(TpsTravailChefExpl)) ~ "Non réponse"
  )) |>
  group_by(`Temps de travail du chef d'exploitation`) |>
  summarise(count = n()) |>
  mutate(`En %` = round(count / sum(count) * 100, 1)) |>
  select(`Temps de travail du chef d'exploitation`, `En %`)

## Surface bio ou non / archipel
surfacesCulturesBioNon <- rga23_surfacesCultures |>
  mutate(TypeCulture = case_when(
    (TypeCulture == 10) ~ "10 - Cultures maraîchères",
    (TypeCulture == 20) ~ "20 - Cultures vivrières",
    (TypeCulture == 30) ~ "30 - Cultures fruitières (hors pépinères) et bois d'oeuvre",
    (TypeCulture == 40) ~ "40 - Feuillages et cultures florales (hors pépinières)",
    (TypeCulture == 50) ~ "50 - Plantes aromatiques, stimulantes et médicinales",
    (TypeCulture == 60) ~ "60 - Pépinières (plantes vendues en pot)",
    (TypeCulture == 70) ~ "70 - Cultures fourragères",
    (TypeCulture == 80) ~ "80 - Jachères",
    TRUE ~ as.character(TypeCulture)
  )) |>
  group_by(TypeCulture) |>
  mutate(surfaceBioCult = case_when(
    SurfaceBio == 1 ~ SurfaceCult,
    TRUE ~ 0
  )) |>
  summarize(
    `Nombre d'exploitants` = n_distinct(interview__key),
    `Surface Totale BIO (Ha)` = round((sum(surfaceBioCult, na.rm = TRUE) / 10000), 1),
    `Surface Totale (Ha)` = round((sum(SurfaceCult, na.rm = TRUE) / 10000), 1),
    `Surface moyenne (m²)` = round(mean(SurfaceCult, na.rm = TRUE), 0)
  )

surfacesJardinsOceaniensBioNon <- rga23_prodVegetales |>
  filter(ModesProduction__4 == 1) |>
  mutate(SurfaceBioJardins = case_when(
    SurfaceBioJardins == 1 ~ SurfaceJardins,
    TRUE ~ 0
  )) |>
  summarize(
    `Nombre d'exploitants` = n_distinct(interview__key),
    `Surface Totale Bio (Ha)` = round((sum(SurfaceBioJardins, na.rm = TRUE) / 10000), 1),
    `Surface Totale (Ha)` = round((sum(SurfaceJardins, na.rm = TRUE) / 10000), 1),
    `Surface moyenne (m²)` = round(mean(SurfaceJardins, na.rm = TRUE), 0)
  )

## Coprahculteurs
coprahculteursSommes <- rga23_coprahculteurs |>
  summarize(
    NbCocoExploitees = sum(NbCocoteraies, na.rm = TRUE),
    NbCocoExploiteesPP = sum(nbCocoStatut1, na.rm = TRUE),
    NbCocoExploiteesPI = sum(nbCocoStatut2, na.rm = TRUE),
    NbCocoExploiteesE = sum(nbCocoStatut3, na.rm = TRUE),
    NbMoyCoco = round(mean(NbCocoteraies, na.rm = TRUE), 2),
    NbMinCoco = min(NbCocoteraies, na.rm = TRUE),
    NbMaxCoco = max(NbCocoteraies, na.rm = TRUE)
  )

## Utilisation produits phyto
produitsPhyto <- rga23_exploitations |>
  filter(eligibilite == 1) |>
  mutate(`Utilisation produits phyto` = case_when(
    UtilisationPhytosanit == 1 ~ "OUI",
    UtilisationPhytosanit == 2 ~ "NON"
  )) |>
  group_by(`Utilisation produits phyto`) |>
  summarise(n = n()) |>
  mutate(`En %` = round((n / sum(n) * 100), 2)) |>
  select(!n)

## Utilisation engrais
engrais <- rga23_exploitations |>
  filter(RaisonsRecensement__1 == 1 & eligibilite == 1) |>
  mutate(`Utilisation engrais` = case_when(
    UtilisationEngrais == 1 ~ "OUI",
    UtilisationEngrais == 2 ~ "NON"
  )) |>
  group_by(`Utilisation engrais`) |>
  summarise(n = n()) |>
  mutate(`En %` = round((n / sum(n) * 100), 2)) |>
  select(!n)

rmarkdown::render("analyse/publicationDoubleTimbre.Rmd", encoding = "UTF-8")

rm(coExploitants, mainOeuvre, moPermFamiliale, poules, rga23_champ_date, rga23A, rga23A_valides, surfacesCultures, surfacesCulturesEligibles, table)
