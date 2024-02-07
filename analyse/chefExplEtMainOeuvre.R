## Chefs d'exploitations

genreChef <- rga23_general |>
  mutate(homme = case_when(SexeChefExpl == 1 ~ 0, SexeChefExpl == 2 ~ 1), femme = case_when(SexeChefExpl == 1 ~ 1, SexeChefExpl == 2 ~ 0)) |>
  summarize(
    NbHommes = sum(homme, na.rm = TRUE),
    NbFemmes = sum(femme, na.rm = TRUE),
    TauxFemmes = NbFemmes / NbHommes
  )

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


formNAChefExpl <- rga23_general |>
  filter(!is.na(FormNAChefExpl)) |>
  mutate(`Formation générale non agricole du chef d'exploitation` = case_when(
    (FormNAChefExpl == 1) ~ "1 : Aucune",
    (FormNAChefExpl == 2) ~ "2 : Primaire",
    (FormNAChefExpl == 3) ~ "3 : Secondaire court",
    (FormNAChefExpl == 4) ~ "4 : Secondaire long",
    (FormNAChefExpl == 5) ~ "5 : Supérieure",
    (is.na(FormNAChefExpl)) ~ "Non réponse"
  )) |>
  group_by(`Formation générale non agricole du chef d'exploitation`) |>
  summarise(count = n()) |>
  mutate(`En %` = round(count / sum(count) * 100, 1)) |>
  select(`Formation générale non agricole du chef d'exploitation`, `En %`)

formAgriChefExpl <- rga23_general |>
  filter(!is.na(FormNAChefExpl)) |>
  mutate(`Formation générale agricole du chef d'exploitation` = case_when(
    (FormAgriChefExpl == 1) ~ "1 : Aucune / Formation sur le tas",
    (FormAgriChefExpl == 2) ~ "2 : MFR, CJA sans obtention de diplôme",
    (FormAgriChefExpl == 3) ~ "3 : Secondaire courte (CAPA, BEPA)",
    (FormAgriChefExpl == 4) ~ "4 : Secondaire longue (BTA, Bac agricole)",
    (FormAgriChefExpl == 5) ~ "5 : Supérieure courte (BTSA)",
    (FormAgriChefExpl == 6) ~ "6 : Supérieure longue (ingénieur)",
    (is.na(FormAgriChefExpl)) ~ "Non réponse"
  )) |>
  group_by(`Formation générale agricole du chef d'exploitation`) |>
  summarise(count = n()) |>
  mutate(`En %` = round(count / sum(count) * 100, 1)) |>
  select(`Formation générale agricole du chef d'exploitation`, `En %`)

## Main d'oeuvre

nbCoexploitants <- rga23_mainOeuvre |> summarize(sum(NbCoExploitants, na.rm = TRUE))

nbMOPermFamiliale <- rga23_mainOeuvre |> summarize(sum(NbMOPermFamiliale, na.rm = TRUE))

nbMOPermNonFamiliale <- (rga23_mainOeuvre |> summarize(sum(nbFemmesNFPerm, na.rm = TRUE))) + (rga23_mainOeuvre |> summarize(sum(nbHommesNFPerm, na.rm = TRUE)))

nbMOOccasionnelle <- rga23_mainOeuvre |> summarize(sum(totalMAOccas, na.rm = TRUE))

## Part de l'agriculture dans les revenus

partRevenusAgriculture <- rga23_tape |>
  filter(!is.na(PartRevenusAgriExpl)) |>
  mutate(`Part de l'agriculture dans les revenus` = case_when(
    (PartRevenusAgriExpl == 1) ~ "1 : 0 à 25%",
    (PartRevenusAgriExpl == 2) ~ "2 : 25 à 50%",
    (PartRevenusAgriExpl == 3) ~ "3 : 50 à 75%",
    (PartRevenusAgriExpl == 4) ~ "4 : Plus de 75%",
    (is.na(PartRevenusAgriExpl)) ~ "Non réponse"
  )) |>
  group_by(`Part de l'agriculture dans les revenus`) |>
  summarise(count = n()) |>
  mutate(`En %` = round(count / sum(count) * 100, 1)) |>
  select(`Part de l'agriculture dans les revenus`, `En %`)

