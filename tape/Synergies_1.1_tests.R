scoreIntegrationNbAnimaux <- left_join(rga23_exploitations, rga23_prodAnimales_NbEtPoids, by = c("interview__key")) |>
  mutate(score = case_when(
    nbEspecesHorsAbeilles == 0 ~ 0,
    ((is.na(AutAlimAnimauxBasseCour) | AutAlimAnimauxBasseCour == 6 | nbAnimauxBasseCour / nbTotalAnimaux < 0.3) &
       (is.na(AutAlimBovinsFourrage) | AutAlimBovinsFourrage == 6 | nbTotalBovins / nbTotalAnimaux < 0.3) &
       (is.na(AutAlimCaprinsFourrage) | AutAlimCaprinsFourrage == 6 | nbTotalCaprins / nbTotalAnimaux < 0.3) &
       (is.na(AutAlimEquidesFourrages) | AutAlimEquidesFourrages == 6 | nbTotalEquides / nbTotalAnimaux < 0.3) &
       (is.na(AutAlimOvinsFourrage) | AutAlimOvinsFourrage == 6 | nbTotalOvins / nbTotalAnimaux < 0.3) &
       (is.na(AutAlimPorcins) | AutAlimPorcins == 6 | nbTotalPorcs / nbTotalAnimaux < 0.3) &
       (is.na(AutAlimPoules) | AutAlimPoules == 6 | nbPoulesPondeuses / nbTotalAnimaux < 0.3)) &
      PropRecyclEngraisOrga == 1 ~ 0,
    ((is.na(AutAlimAnimauxBasseCour) | AutAlimAnimauxBasseCour == 5 | AutAlimAnimauxBasseCour == 4 | nbAnimauxBasseCour / nbTotalAnimaux < 0.3) &
       (is.na(AutAlimBovinsFourrage) | AutAlimBovinsFourrage == 5 | AutAlimBovinsFourrage == 4 | nbTotalBovins / nbTotalAnimaux < 0.3) &
       (is.na(AutAlimCaprinsFourrage) | AutAlimCaprinsFourrage == 5 | AutAlimCaprinsFourrage == 4 | nbTotalCaprins / nbTotalAnimaux < 0.3) &
       (is.na(AutAlimEquidesFourrages) | AutAlimEquidesFourrages == 5 | AutAlimEquidesFourrages == 4 | nbTotalEquides / nbTotalAnimaux < 0.3) &
       (is.na(AutAlimOvinsFourrage) | AutAlimOvinsFourrage == 5 | AutAlimOvinsFourrage == 4 | nbTotalOvins / nbTotalAnimaux < 0.3) &
       (is.na(AutAlimPorcins) | AutAlimPorcins == 5 | AutAlimPorcins == 4 | nbTotalPorcs / nbTotalAnimaux < 0.3) &
       (is.na(AutAlimPoules) | AutAlimPoules == 5 | AutAlimPoules == 4 | nbPoulesPondeuses / nbTotalAnimaux < 0.3)) &
      (PropRecyclEngraisOrga == 1 | PropRecyclEngraisOrga == 2) ~ 1,
    ((is.na(AutAlimAnimauxBasseCour) | AutAlimAnimauxBasseCour == 3 | nbAnimauxBasseCour / nbTotalAnimaux < 0.3) &
       (is.na(AutAlimBovinsFourrage) | AutAlimBovinsFourrage == 3 | nbTotalBovins / nbTotalAnimaux < 0.3) &
       (is.na(AutAlimCaprinsFourrage) | AutAlimCaprinsFourrage == 3 | nbTotalCaprins / nbTotalAnimaux < 0.3) &
       (is.na(AutAlimEquidesFourrages) | AutAlimEquidesFourrages == 3 | nbTotalEquides / nbTotalAnimaux < 0.3) &
       (is.na(AutAlimOvinsFourrage) | AutAlimOvinsFourrage == 3 | nbTotalOvins / nbTotalAnimaux < 0.3) &
       (is.na(AutAlimPorcins) | AutAlimPorcins == 3 | nbTotalPorcs / nbTotalAnimaux < 0.3) &
       (is.na(AutAlimPoules) | AutAlimPoules == 3 | nbPoulesPondeuses / nbTotalAnimaux < 0.3)) &
      (PropRecyclEngraisOrga == 2 | PropRecyclEngraisOrga == 3) ~ 2,
    ((is.na(AutAlimAnimauxBasseCour) | AutAlimAnimauxBasseCour == 2 | nbAnimauxBasseCour / nbTotalAnimaux < 0.3) &
       (is.na(AutAlimBovinsFourrage) | AutAlimBovinsFourrage == 2 | nbTotalBovins / nbTotalAnimaux < 0.3) &
       (is.na(AutAlimCaprinsFourrage) | AutAlimCaprinsFourrage == 2 | nbTotalCaprins / nbTotalAnimaux < 0.3) &
       (is.na(AutAlimEquidesFourrages) | AutAlimEquidesFourrages == 2 | nbTotalEquides / nbTotalAnimaux < 0.3) &
       (is.na(AutAlimOvinsFourrage) | AutAlimOvinsFourrage == 2 | nbTotalOvins / nbTotalAnimaux < 0.3) &
       (is.na(AutAlimPorcins) | AutAlimPorcins == 2 | nbTotalPorcs / nbTotalAnimaux < 0.3) &
       (is.na(AutAlimPoules) | AutAlimPoules == 2 | nbPoulesPondeuses / nbTotalAnimaux < 0.3)) &
      (PropRecyclEngraisOrga == 3 | PropRecyclEngraisOrga == 4) ~ 3,
    ((is.na(AutAlimAnimauxBasseCour) | AutAlimAnimauxBasseCour == 1 | nbAnimauxBasseCour / nbTotalAnimaux < 0.3) &
       (is.na(AutAlimBovinsFourrage) | AutAlimBovinsFourrage == 1 | nbTotalBovins / nbTotalAnimaux < 0.3) &
       (is.na(AutAlimCaprinsFourrage) | AutAlimCaprinsFourrage == 1 | nbTotalCaprins / nbTotalAnimaux < 0.3) &
       (is.na(AutAlimEquidesFourrages) | AutAlimEquidesFourrages == 1 | nbTotalEquides / nbTotalAnimaux < 0.3) &
       (is.na(AutAlimOvinsFourrage) | AutAlimOvinsFourrage == 1 | nbTotalOvins / nbTotalAnimaux < 0.3) &
       (is.na(AutAlimPorcins) | AutAlimPorcins == 1 | nbTotalPorcs / nbTotalAnimaux < 0.3) &
       (is.na(AutAlimPoules) | AutAlimPoules == 1 | nbPoulesPondeuses / nbTotalAnimaux < 0.3)) &
      (PropRecyclEngraisOrga == 4) ~ 4,
    TRUE ~ 5
  ))

scoreIntegrationPoidsAnimaux <- left_join(rga23_exploitations, rga23_prodAnimales_NbEtPoids, by = c("interview__key")) |>
  mutate(score = case_when(
    nbEspecesHorsAbeilles == 0 ~ 0,
    ((is.na(AutAlimAnimauxBasseCour) | AutAlimAnimauxBasseCour == 6 | poidsAnimauxBasseCour / poidsTotalAnimaux < 0.3) &
       (is.na(AutAlimBovinsFourrage) | AutAlimBovinsFourrage == 6 | poidsBovins / poidsTotalAnimaux < 0.3) &
       (is.na(AutAlimCaprinsFourrage) | AutAlimCaprinsFourrage == 6 | poidsCaprins / poidsTotalAnimaux < 0.3) &
       (is.na(AutAlimEquidesFourrages) | AutAlimEquidesFourrages == 6 | poidsEquides / poidsTotalAnimaux < 0.3) &
       (is.na(AutAlimOvinsFourrage) | AutAlimOvinsFourrage == 6 | poidsOvins / poidsTotalAnimaux < 0.3) &
       (is.na(AutAlimPorcins) | AutAlimPorcins == 6 | poidsPorcins / poidsTotalAnimaux < 0.3) &
       (is.na(AutAlimPoules) | AutAlimPoules == 6 | poidsPoules / poidsTotalAnimaux < 0.3)) &
      PropRecyclEngraisOrga == 1 ~ 0,
    ((is.na(AutAlimAnimauxBasseCour) | AutAlimAnimauxBasseCour == 5 | AutAlimAnimauxBasseCour == 4 | poidsAnimauxBasseCour / poidsTotalAnimaux < 0.3) &
       (is.na(AutAlimBovinsFourrage) | AutAlimBovinsFourrage == 5 | AutAlimBovinsFourrage == 4 | poidsBovins / poidsTotalAnimaux < 0.3) &
       (is.na(AutAlimCaprinsFourrage) | AutAlimCaprinsFourrage == 5 | AutAlimCaprinsFourrage == 4 | poidsCaprins / poidsTotalAnimaux < 0.3) &
       (is.na(AutAlimEquidesFourrages) | AutAlimEquidesFourrages == 5 | AutAlimEquidesFourrages == 4 | poidsEquides / poidsTotalAnimaux < 0.3) &
       (is.na(AutAlimOvinsFourrage) | AutAlimOvinsFourrage == 5 | AutAlimOvinsFourrage == 4 | poidsOvins / poidsTotalAnimaux < 0.3) &
       (is.na(AutAlimPorcins) | AutAlimPorcins == 5 | AutAlimPorcins == 4 | poidsPorcins / poidsTotalAnimaux < 0.3) &
       (is.na(AutAlimPoules) | AutAlimPoules == 5 | AutAlimPoules == 4 | poidsPoules / poidsTotalAnimaux < 0.3)) &
      (PropRecyclEngraisOrga == 1 | PropRecyclEngraisOrga == 2) ~ 1,
    ((is.na(AutAlimAnimauxBasseCour) | AutAlimAnimauxBasseCour == 3 | poidsAnimauxBasseCour / poidsTotalAnimaux < 0.3) &
       (is.na(AutAlimBovinsFourrage) | AutAlimBovinsFourrage == 3 | poidsBovins / poidsTotalAnimaux < 0.3) &
       (is.na(AutAlimCaprinsFourrage) | AutAlimCaprinsFourrage == 3 | poidsCaprins / poidsTotalAnimaux < 0.3) &
       (is.na(AutAlimEquidesFourrages) | AutAlimEquidesFourrages == 3 | poidsEquides / poidsTotalAnimaux < 0.3) &
       (is.na(AutAlimOvinsFourrage) | AutAlimOvinsFourrage == 3 | poidsOvins / poidsTotalAnimaux < 0.3) &
       (is.na(AutAlimPorcins) | AutAlimPorcins == 3 | poidsPorcins / poidsTotalAnimaux < 0.3) &
       (is.na(AutAlimPoules) | AutAlimPoules == 3 | poidsPoules / poidsTotalAnimaux < 0.3)) &
      (PropRecyclEngraisOrga == 2 | PropRecyclEngraisOrga == 3) ~ 2,
    ((is.na(AutAlimAnimauxBasseCour) | AutAlimAnimauxBasseCour == 2 | poidsAnimauxBasseCour / poidsTotalAnimaux < 0.3) &
       (is.na(AutAlimBovinsFourrage) | AutAlimBovinsFourrage == 2 | poidsBovins / poidsTotalAnimaux < 0.3) &
       (is.na(AutAlimCaprinsFourrage) | AutAlimCaprinsFourrage == 2 | poidsCaprins / poidsTotalAnimaux < 0.3) &
       (is.na(AutAlimEquidesFourrages) | AutAlimEquidesFourrages == 2 | poidsEquides / poidsTotalAnimaux < 0.3) &
       (is.na(AutAlimOvinsFourrage) | AutAlimOvinsFourrage == 2 | poidsOvins / poidsTotalAnimaux < 0.3) &
       (is.na(AutAlimPorcins) | AutAlimPorcins == 2 | poidsPorcins / poidsTotalAnimaux < 0.3) &
       (is.na(AutAlimPoules) | AutAlimPoules == 2 | poidsPoules / poidsTotalAnimaux < 0.3)) &
      (PropRecyclEngraisOrga == 3 | PropRecyclEngraisOrga == 4) ~ 3,
    ((is.na(AutAlimAnimauxBasseCour) | AutAlimAnimauxBasseCour == 1 | poidsAnimauxBasseCour / poidsTotalAnimaux < 0.3) &
       (is.na(AutAlimBovinsFourrage) | AutAlimBovinsFourrage == 1 | poidsBovins / poidsTotalAnimaux < 0.3) &
       (is.na(AutAlimCaprinsFourrage) | AutAlimCaprinsFourrage == 1 | poidsCaprins / poidsTotalAnimaux < 0.3) &
       (is.na(AutAlimEquidesFourrages) | AutAlimEquidesFourrages == 1 | poidsEquides / poidsTotalAnimaux < 0.3) &
       (is.na(AutAlimOvinsFourrage) | AutAlimOvinsFourrage == 1 | poidsOvins / poidsTotalAnimaux < 0.3) &
       (is.na(AutAlimPorcins) | AutAlimPorcins == 1 | poidsPorcins / poidsTotalAnimaux < 0.3) &
       (is.na(AutAlimPoules) | AutAlimPoules == 1 | poidsPoules / poidsTotalAnimaux < 0.3)) &
      (PropRecyclEngraisOrga == 4) ~ 4,
    TRUE ~ 5
  ))

scoreIntegrationNbAnimaux |>
  group_by(score) |>
  count()
scoreIntegrationPoidsAnimaux |>
  group_by(score) |>
  count()

