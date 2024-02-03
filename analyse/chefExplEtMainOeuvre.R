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