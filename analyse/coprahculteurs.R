## Coprahculteurs

## Conservation des coprahculteurs eligibles

rga23_coprahculteurs <- rga23_coprahculteurs |>
  filter(eligibiliteCoprah == 1)

coprahculteursSommes <- rga23_coprahculteurs |>
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
  mutate(`Part de la production de coprah dans les revenus annuels` = case_when(
    (PartRevenusCoprahExpl == 1) ~ "1 : 0 à 25%",
    (PartRevenusCoprahExpl == 2) ~ "2 : 25 à 50%",
    (PartRevenusCoprahExpl == 3) ~ "3 : 50 à 75%",
    (PartRevenusCoprahExpl == 4) ~ "4 : Plus de 75%",
    (is.na(PartRevenusCoprahExpl)) ~ "Non réponse"
  )) |>
  group_by(`Part de la production de coprah dans les revenus annuels`) |>
  calculPourcentage()


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

# Séchoir solaire....................................1
rga23_coprahculteurs |>
  group_by(SechoirCoprah__1) |>
  calculPourcentage()
# Séchoir thermique..................................2
rga23_coprahculteurs |>
  group_by(SechoirCoprah__2) |>
  calculPourcentage()
# Aucun séchoir (séchage sur bâche au soleil, ...)...3
rga23_coprahculteurs |>
  group_by(SechoirCoprah__3) |>
  calculPourcentage()

rga23_coprahculteurs |> summarize(
  nbCocoEntretenues1 = sum(EntretienCoco1, na.rm = TRUE),
  nbCoco1 = sum(nbCocoStatut1, na.rm = TRUE),
  proportion1 = nbCocoEntretenues1 / nbCoco1 * 100,
  nbCocoEntretenues2 = sum(EntretienCoco2, na.rm = TRUE),
  nbCoco2 = sum(nbCocoStatut2, na.rm = TRUE),
  proportion2 = nbCocoEntretenues2 / nbCoco2 * 100,
  nbCocoEntretenues3 = sum(EntretienCoco3, na.rm = TRUE),
  nbCoco3 = sum(nbCocoStatut3, na.rm = TRUE),
  proportion3 = nbCocoEntretenues3 / nbCoco3 * 100,
)

# modeEntretienCocoteraieSelonStatut <- function(nbCocoEntretenues, nbCocoStatut, modeEntretien) {
#   rga23_coprahculteurs |>
#     filter({{ nbCocoStatut }} > 0) |>
#     group_by({{ modeEntretien }}) |>
#     summarize(
#       sommeCocoEntretenues = sum({{ nbCocoEntretenues }}),
#       sommeCocoStatut = sum({{ nbCocoStatut }})
#     )
# }
# 
# modeEntretienCocoteraieSelonStatut(EntretienCoco1, nbCocoStatut1, ModeEntretCoco1__1)
# modeEntretienCocoteraieSelonStatut(EntretienCoco2, nbCocoStatut2, ModeEntretCoco2__1)
# modeEntretienCocoteraieSelonStatut(EntretienCoco3, nbCocoStatut3, ModeEntretCoco3__1)
# 
# modeEntretienCocoteraieSelonStatut(EntretienCoco1, nbCocoStatut1, ModeEntretCoco1__2)
# modeEntretienCocoteraieSelonStatut(EntretienCoco2, nbCocoStatut2, ModeEntretCoco2__2)
# modeEntretienCocoteraieSelonStatut(EntretienCoco3, nbCocoStatut3, ModeEntretCoco3__2)
# 
# modeEntretienCocoteraieSelonStatut(EntretienCoco1, nbCocoStatut1, ModeEntretCoco1__3)
# modeEntretienCocoteraieSelonStatut(EntretienCoco2, nbCocoStatut2, ModeEntretCoco2__3)
# modeEntretienCocoteraieSelonStatut(EntretienCoco3, nbCocoStatut3, ModeEntretCoco3__3)

rga23_coprahculteurs |> summarize(
  sommeCocoEntretenues_1 = sum(ifelse((!is.na(ModeEntretCoco1__1) & ModeEntretCoco1__1 == 1), EntretienCoco1, 0)+
                                 ifelse((!is.na(ModeEntretCoco2__1) & ModeEntretCoco2__1 == 1), EntretienCoco2, 0) + 
                                ifelse((!is.na(ModeEntretCoco3__1) & ModeEntretCoco3__1 == 1), EntretienCoco3, 0)),
  sommeCocoEntretenues_2 = sum(ifelse((!is.na(ModeEntretCoco1__2) & ModeEntretCoco1__2 == 1), EntretienCoco1, 0)+
                                 ifelse((!is.na(ModeEntretCoco2__2) & ModeEntretCoco2__2 == 1), EntretienCoco2, 0) + 
                                 ifelse((!is.na(ModeEntretCoco3__2) & ModeEntretCoco3__2 == 1), EntretienCoco3, 0)),
  sommeCocoEntretenues_3 = sum(ifelse((!is.na(ModeEntretCoco1__3) & ModeEntretCoco1__3 == 1), EntretienCoco1, 0)+
                                 ifelse((!is.na(ModeEntretCoco2__3) & ModeEntretCoco2__3 == 1), EntretienCoco2, 0) + 
                                 ifelse((!is.na(ModeEntretCoco3__3) & ModeEntretCoco3__3 == 1), EntretienCoco3, 0)),
  sommeCocoEntretenues = sum(ifelse(!is.na(EntretienCoco1), EntretienCoco1, 0)+
                               ifelse(!is.na(EntretienCoco2), EntretienCoco2, 0) + 
                               ifelse(!is.na(EntretienCoco3), EntretienCoco3, 0)),
  prop1 = sommeCocoEntretenues_1 / sommeCocoEntretenues * 100,
  prop2 = sommeCocoEntretenues_2 / sommeCocoEntretenues * 100,
  prop3 = sommeCocoEntretenues_3 / sommeCocoEntretenues * 100
)

rga23_cocoteraies |> group_by(cocoteraieBaguee) |> calculPourcentage()

