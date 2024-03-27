## Coprahculteurs

Partie5_nombreCoprahculteurs <- rga23_coprahculteurs |>
  group_by(indicRGA23_Coprah) |>
  count()
writeCSV(Partie5_nombreCoprahculteurs)

Partie5_comptagesCocoteraies <- rga23_coprahculteurs |>
  summarize(
    NbCocoExploitees = sum(NbCocoteraies, na.rm = TRUE),
    NbCocoExploiteesPP = sum(nbCocoStatut1, na.rm = TRUE),
    PropCocoExploiteesPP = round(sum(nbCocoStatut1, na.rm = TRUE) / NbCocoExploitees*100),
    NbCocoEntretenuesPP = sum(EntretienCoco1, na.rm = TRUE),
    NbCocoExploiteesPI = sum(nbCocoStatut2, na.rm = TRUE),
    PropCocoExploiteesPI = round(sum(nbCocoStatut2, na.rm = TRUE) / NbCocoExploitees*100),
    NbCocoEntretenuesPI = sum(EntretienCoco2, na.rm = TRUE),
    NbCocoExploiteesE = sum(nbCocoStatut3, na.rm = TRUE),
    PropCocoExploiteesE = round(sum(nbCocoStatut3, na.rm = TRUE) / NbCocoExploitees*100),
    NbCocoEntretenuesE = sum(EntretienCoco3, na.rm = TRUE),
    NbMoyCoco = round(mean(NbCocoteraies, na.rm = TRUE), 2),
    NbMinCoco = min(NbCocoteraies, na.rm = TRUE),
    NbMaxCoco = max(NbCocoteraies, na.rm = TRUE)
  )
writeCSV(Partie5_comptagesCocoteraies)

Partie5_partRevenusCoprah <- rga23_coprahculteurs |>
  mutate(`Part de la production de coprah dans les revenus annuels` = case_when(
    (PartRevenusCoprahExpl == 1) ~ "1 : 0 à 25%",
    (PartRevenusCoprahExpl == 2) ~ "2 : 25 à 50%",
    (PartRevenusCoprahExpl == 3) ~ "3 : 50 à 75%",
    (PartRevenusCoprahExpl == 4) ~ "4 : Plus de 75%",
    (is.na(PartRevenusCoprahExpl)) ~ "Non réponse"
  )) |>
  group_by(`Part de la production de coprah dans les revenus annuels`) |>
  calculPourcentage()
writeCSV(Partie5_partRevenusCoprah)

Partie5_nbCocoteraiesConservationToutRevenu <- rga23_cocoteraies |>
  filter(PartCoco == 100) |>
  count()
writeCSV(Partie5_nbCocoteraiesConservationToutRevenu)

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

nbCocoExploiteesE <- Partie5_comptagesCocoteraies$NbCocoExploiteesE
nbCocoExploiteesPI <- Partie5_comptagesCocoteraies$NbCocoExploiteesPI
nbCocoExploiteesPP <- Partie5_comptagesCocoteraies$NbCocoExploiteesPP

pivotNbCocoteraiesPartInf100 <- nbCocoteraiesPartInf100 |>
  pivot_wider(names_from = Statut, values_from = n, values_fill = 0)

totaux <- pivotNbCocoteraiesPartInf100 |>
  summarise(across(starts_with("Cocoteraies"), sum))

Partie5_cocoteraiesStatutPartRevenu <- pivotNbCocoteraiesPartInf100 |>
  mutate(across(starts_with("Cocoteraies"), ~ round(. / totaux[[cur_column()]] * 100, 1))) |>
  rename_with(~ paste0(., "- en %"), starts_with("Cocoteraies"))
writeCSV(Partie5_cocoteraiesStatutPartRevenu)

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

Partie5_replanter <- producteursProprietairePlein |>
  left_join(producteursProprietaireIndivision, by = "Action") |>
  left_join(producteursExploitants, by = "Action") |>
  mutate(`Plantation de nouveaux cocotiers` = ifelse(Action == 1, "OUI", "NON")) |>
  select(
    `Plantation de nouveaux cocotiers`,
    `Producteurs en tant que Propriétaire plein - En %`,
    `Producteurs en tant que Propriétaire en indivision - En %`,
    `Producteurs en tant que Exploitant - En %`
  )
writeCSV(Partie5_replanter)

## Supprimer anciens cocotiers

producteursProprietairePlein <- actionParStatut(nbCocoStatut1, SuppAnciensCocotiers1, "Producteurs en tant que Propriétaire plein - En %")
producteursProprietaireIndivision <- actionParStatut(nbCocoStatut2, SuppAnciensCocotiers2, "Producteurs en tant que Propriétaire en indivision - En %")
producteursExploitants <- actionParStatut(nbCocoStatut3, SuppAnciensCocotiers3, "Producteurs en tant que Exploitant - En %")

Partie5_supprimer <- producteursProprietairePlein |>
  left_join(producteursProprietaireIndivision, by = "Action") |>
  left_join(producteursExploitants, by = "Action") |>
  mutate(`Suppression des anciens cocotiers` = ifelse(Action == 1, "OUI", "NON")) |>
  select(
    `Suppression des anciens cocotiers`,
    `Producteurs en tant que Propriétaire plein - En %`,
    `Producteurs en tant que Propriétaire en indivision - En %`,
    `Producteurs en tant que Exploitant - En %`
  )
writeCSV(Partie5_supprimer)

# Séchoir solaire....................................1
sechoirSolaire <- rga23_coprahculteurs |>
  mutate(
    Type = "Séchoir Solaire",
    Utilisation = case_when(
      SechoirCoprah__1 == 0 ~ "Non",
      SechoirCoprah__1 == 1 ~ "Oui"
    )
  ) |>
  group_by(Type, Utilisation) |>
  calculPourcentage()
# Séchoir thermique..................................2
sechoirThermique <- rga23_coprahculteurs |>
  mutate(
    Type = "Séchoir thermique",
    Utilisation = case_when(
      SechoirCoprah__2 == 0 ~ "Non",
      SechoirCoprah__2 == 1 ~ "Oui"
    )
  ) |>
  group_by(Type, Utilisation) |>
  calculPourcentage()
# Aucun séchoir (séchage sur bâche au soleil, ...)...3
aucunSechoir <- rga23_coprahculteurs |>
  mutate(
    Type = "Aucun séchoir (séchage sur bâche au soleil, ...)",
    Utilisation = case_when(
      SechoirCoprah__3 == 0 ~ "Non",
      SechoirCoprah__3 == 1 ~ "Oui"
    )
  ) |>
  group_by(Type, Utilisation) |>
  calculPourcentage()
Partie5_sechoirs <- rbind(sechoirSolaire, sechoirThermique, aucunSechoir) |>
  pivot_wider(names_from = Utilisation, values_from = `En %`)
writeCSV(Partie5_sechoirs)

Partie5_propCocoEntretenuesStatut <- rga23_coprahculteurs |>
  summarize(
    nbCocoEntretenues1 = sum(EntretienCoco1, na.rm = TRUE),
    nbCoco1 = sum(nbCocoStatut1, na.rm = TRUE),
    propCocoEntretenuesPP = nbCocoEntretenues1 / nbCoco1 * 100,
    nbCocoEntretenues2 = sum(EntretienCoco2, na.rm = TRUE),
    nbCoco2 = sum(nbCocoStatut2, na.rm = TRUE),
    propCocoEntretenuesPI = nbCocoEntretenues2 / nbCoco2 * 100,
    nbCocoEntretenues3 = sum(EntretienCoco3, na.rm = TRUE),
    nbCoco3 = sum(nbCocoStatut3, na.rm = TRUE),
    propCocoEntretenuesE = nbCocoEntretenues3 / nbCoco3 * 100,
  ) |>
  select(propCocoEntretenuesPP, propCocoEntretenuesPI, propCocoEntretenuesE)
writeCSV(Partie5_propCocoEntretenuesStatut)

Partie5_propCocoEntretenuesMode <- rga23_coprahculteurs |>
  summarize(
    sommeCocoEntretenues_Mode1 = sum(ifelse((!is.na(ModeEntretCoco1__1) & ModeEntretCoco1__1 == 1), EntretienCoco1, 0) +
      ifelse((!is.na(ModeEntretCoco2__1) & ModeEntretCoco2__1 == 1), EntretienCoco2, 0) +
      ifelse((!is.na(ModeEntretCoco3__1) & ModeEntretCoco3__1 == 1), EntretienCoco3, 0)),
    sommeCocoEntretenues_Mode2 = sum(ifelse((!is.na(ModeEntretCoco1__2) & ModeEntretCoco1__2 == 1), EntretienCoco1, 0) +
      ifelse((!is.na(ModeEntretCoco2__2) & ModeEntretCoco2__2 == 1), EntretienCoco2, 0) +
      ifelse((!is.na(ModeEntretCoco3__2) & ModeEntretCoco3__2 == 1), EntretienCoco3, 0)),
    sommeCocoEntretenues_Mode3 = sum(ifelse((!is.na(ModeEntretCoco1__3) & ModeEntretCoco1__3 == 1), EntretienCoco1, 0) +
      ifelse((!is.na(ModeEntretCoco2__3) & ModeEntretCoco2__3 == 1), EntretienCoco2, 0) +
      ifelse((!is.na(ModeEntretCoco3__3) & ModeEntretCoco3__3 == 1), EntretienCoco3, 0)),
    sommeCocoEntretenues = sum(ifelse(!is.na(EntretienCoco1), EntretienCoco1, 0) +
      ifelse(!is.na(EntretienCoco2), EntretienCoco2, 0) +
      ifelse(!is.na(EntretienCoco3), EntretienCoco3, 0)),
    propCocoterairesEntretenuesMode1 = sommeCocoEntretenues_Mode1 / sommeCocoEntretenues * 100,
    propCocoterairesEntretenuesMode2 = sommeCocoEntretenues_Mode2 / sommeCocoEntretenues * 100,
    propCocoterairesEntretenuesMode3 = sommeCocoEntretenues_Mode3 / sommeCocoEntretenues * 100
  ) |>
  select(propCocoterairesEntretenuesMode1, propCocoterairesEntretenuesMode2, propCocoterairesEntretenuesMode3)
writeCSV(Partie5_propCocoEntretenuesMode)

Partie5_propBaguees <- rga23_cocoteraies |>
  group_by(cocoteraieBaguee) |>
  calculPourcentage()
writeCSV(Partie5_propBaguees)
