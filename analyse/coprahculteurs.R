## Coprahculteurs
coprahculteursSommes <- rga23_coprahculteurs |>
  filter(eligibiliteCoprah == 1) |>
  summarize(
    NbCocoExploitees = sum(NbCocoteraies, na.rm = TRUE),
    NbCocoExploiteesPP = sum(nbCocoStatut1, na.rm = TRUE),
    NbCocoEntretenuesPP = sum(EntretienCoco1, na.rm = TRUE),
    NbCocoExploiteesPI = sum(nbCocoStatut2, na.rm = TRUE),
    NbCocoEntretenuesPI = sum(EntretienCoco2, na.rm = TRUE),
    NbCocoExploiteesE = sum(nbCocoStatut3, na.rm = TRUE),
    NbCocoEntretenuesE = sum(EntretienCoco3, na.rm = TRUE),
    NbMoyCoco = round(mean(NbCocoteraies, na.rm = TRUE), 2),
    NbMinCoco = min(NbCocoteraies, na.rm = TRUE),
    NbMaxCoco = max(NbCocoteraies, na.rm = TRUE)
  )

partRevenusCoprah <- rga23_coprahculteurs |>
  filter(eligibiliteCoprah == 1) |>
  mutate(`Part de la production de coprah dans les revenus annuels` = case_when(
    (PartRevenusCoprahExpl == 1) ~ "1 : 0 à 25%",
    (PartRevenusCoprahExpl == 2) ~ "2 : 25 à 50%",
    (PartRevenusCoprahExpl == 3) ~ "3 : 50 à 75%",
    (PartRevenusCoprahExpl == 4) ~ "4 : Plus de 75%",
    (is.na(PartRevenusCoprahExpl)) ~ "Non réponse"
  )) |>
  group_by(`Part de la production de coprah dans les revenus annuels`) |>
  summarise(count = n()) |>
  mutate(`En %` = round(count / sum(count) * 100, 1)) |>
  select(`Part de la production de coprah dans les revenus annuels`, `En %`)


nbCocoteraiesToutRevenu <- rga23_cocoteraies |>
  filter(PartCoco == 100) |>
  count()

nbCocoteraiesPartInf100 <- rga23_cocoteraies |>
  mutate(
    PartConservee = case_when(
      PartCoco <= 25 ~ "0 à 25%",
      PartCoco <= 50 ~ "26 à 50%",
      PartCoco <= 75 ~ "51 à 75%",
      PartCoco < 100 ~ "76 à 99%",
      PartCoco == 100 ~ "Tout le revenu - 100%"
    ),
    Statut = case_when(
      statutCoco == 1 ~ "Cocoteraies exploitées en tant que Propriétaire plein",
      statutCoco == 2 ~ "Cocoteraies exploitées en tant que Propriétaire en indivision",
      statutCoco == 3 ~ "Cocoteraies exploitées en tant que Exploitant"
    )
  ) |>
  group_by(PartConservee, Statut) |>
  count() |>
  ungroup()

nbCocoExploiteesE <- coprahculteursSommes$NbCocoExploiteesE
nbCocoExploiteesPI <- coprahculteursSommes$NbCocoExploiteesPI
nbCocoExploiteesPP <- coprahculteursSommes$NbCocoExploiteesPP

pivotNbCocoteraiesPartInf100 <- nbCocoteraiesPartInf100 |>
  pivot_wider(names_from = Statut, values_from = n, values_fill = 0) 

totaux <- pivotNbCocoteraiesPartInf100 |>
  summarise(across(starts_with("Cocoteraies"), sum))

cocoteraiesExploiteesPartRevenu <- pivotNbCocoteraiesPartInf100 |>
  mutate(across(starts_with("Cocoteraies"), ~ round(. / totaux[[cur_column()]] * 100, 1))) |>
  rename_with(~ paste0(., "- en %"), starts_with("Cocoteraies"))

# Propriétaire plein

actionParStatut <- function(nbCocoStatut, ActionCocoteraie, label) {
  rga23_coprahculteurs |>
    rename(Action = {{ ActionCocoteraie }}) |>
    filter({{ nbCocoStatut }} > 0 & !is.na(Action)) |>
    group_by(Action) |>
    summarise(count = n()) |>
    mutate({{ label }} := round(count / sum(count) * 100, 1))
}

## Replanter nouveaux cocotiers

producteursProprietairePlein <- actionParStatut(nbCocoStatut1, ReplanterCocoteraie1, "Producteurs en tant que Propriétaire plein - En %")
producteursProprietaireIndivision <- actionParStatut(nbCocoStatut2, ReplanterCocoteraie2, "Producteurs en tant que Propriétaire en indivision - En %")
producteursExploitants <- actionParStatut(nbCocoStatut3, ReplanterCocoteraie3, "Producteurs en tant que Exploitant - En %")

replanter <- producteursProprietairePlein |>
  left_join(producteursProprietaireIndivision, by = "Action") |>
  left_join(producteursExploitants, by = "Action") |>
  mutate(`Plantation de nouveaux cocotiers` = ifelse(Action == 1, "OUI", "NON")) |>
  select(
    `Plantation de nouveaux cocotiers`,
    `Producteurs en tant que Propriétaire plein - En %`,
    `Producteurs en tant que Propriétaire en indivision - En %`,
    `Producteurs en tant que Exploitant - En %`
  )

## Supprimer anciens cocotiers

producteursProprietairePlein <- actionParStatut(nbCocoStatut1, SuppAnciensCocotiers1, "Producteurs en tant que Propriétaire plein - En %")
producteursProprietaireIndivision <- actionParStatut(nbCocoStatut2, SuppAnciensCocotiers2, "Producteurs en tant que Propriétaire en indivision - En %")
producteursExploitants <- actionParStatut(nbCocoStatut3, SuppAnciensCocotiers3, "Producteurs en tant que Exploitant - En %")

supprimer <- producteursProprietairePlein |>
  left_join(producteursProprietaireIndivision, by = "Action") |>
  left_join(producteursExploitants, by = "Action") |>
  mutate(`Suppression des anciens cocotiers` = ifelse(Action == 1, "OUI", "NON")) |>
  select(
    `Suppression des anciens cocotiers`,
    `Producteurs en tant que Propriétaire plein - En %`,
    `Producteurs en tant que Propriétaire en indivision - En %`,
    `Producteurs en tant que Exploitant - En %`
  )
