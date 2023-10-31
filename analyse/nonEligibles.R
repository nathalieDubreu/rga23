rga23 <- readCSV("rga23.csv")

nonEligiblesRGA <- rga23 |>
  filter((interview__status == 100 | interview__status == 120 | interview__status == 130) & statut_collecte == 1) |>
  filter((eligibiliteCoprah == 0 & substring(id_exploitation, 0, 1) == "C") | (eligibilite == 0 & substring(id_exploitation, 0, 1) == "P") | (eligibilite == 0 & substring(id_exploitation, 0, 1) == "M") | (eligibilite == 0 & eligibiliteCoprah == 0 & substring(id_exploitation, 0, 1) == "X"))

# Raison de la non éligibilité

## Personnes physiques et morales ou coprahculteurs
nonEligiblesRGA_PMC <- nonEligiblesRGA |>
  filter(substring(id_exploitation, 0, 1) != "X") |>
  mutate(Raison = case_when(
    RaisonsRecensement__4 == 1 ~ "1. L'exploitation ne fait pas d'agriculture ni d'élevage (et n'est pas un producteur de plus de 2,7 tonnes de coprah)",
    ArretActivite == 1 ~ "2a. Arrêt définitif de l'activité (exploitation agricole)",
    (substring(id_exploitation, 0, 1) == "C" & InterruptionTemporaireCoprah == 2) ~ "2b. Arrêt définitif de la production de coprah",
    AutoConsommation == 1 ~ "3. Autoconsommation de toute la production",
    InstallationRecente == 1 ~ "4. Installation récente pas encore en production",
    ConsommationStockNonFamilial == 2 ~ "5. Aucune utilisation de la production",
    !is.na(RaisonNonProduction) ~ "6. Non production pour une raison non prévue",
    TRUE ~ "Autre ?!?"
  ))

nonEligiblesRGA_PMC |>
  group_by(Raison) |>
  count()

################## TODO -> Vérifier les "Autre ?!?" et les "Aucune utilisation de la production"
aVerifier <- nonEligiblesRGA_PMC |> filter(Raison == "5. Aucune utilisation de la production" & interview__key != "01-00-20-50" & interview__key != "74-92-39-14" & interview__key != "81-75-46-06")

## Double casquette : production de coprah définitivement arrêté + raison identifiée ci-dessous pour la partie exploitation agricole
nonEligiblesRGA_X <- nonEligiblesRGA |>
  filter(substring(id_exploitation, 0, 1) == "X") |>
  mutate(Raison = case_when(
    RaisonsRecensement__4 == 1 ~ "1. L'exploitation ne fait pas d'agriculture ni d'élevage (et n'est pas un producteur de plus de 2,7 tonnes de coprah)",
    ArretActivite == 1 ~ "2. Arrêt définitif de l'activité (exploitation agricole)",
    AutoConsommation == 1 ~ "3. Autoconsommation de toute la production",
    InstallationRecente == 1 ~ "4. Installation récente pas encore en production",
    ConsommationStockNonFamilial == 2 ~ "5. Aucune utilisation de la production",
    !is.na(RaisonNonProduction) ~ "6. Non production pour une raison non prévue",
    TRUE ~ "Autre ?!?"
  ))

################## TODO -> Vérifier les "Autre ?!?" et les "Aucune utilisation de la production"
aVerifier <- nonEligiblesRGA_X |>
  filter(Raison == "5. Aucune utilisation de la production" | Raison == "Autre ?!?") |>
  filter(interview__key != "39-90-06-55" & interview__key != "84-75-99-87")


rm(nonEligiblesRGA, nonEligiblesRGA_PMC, nonEligiblesRGA_X, aVerifier)
