# Bilan des classements

## 1 - Diversité

groupesScoresDiversite <- full_join(
  score_1_Cultures |>
    group_by(score) |>
    summarize(Diversite_1_Culture = n()),
  score_2_Animaux |>
    group_by(score) |>
    summarize(Diversite_2_Animaux = n()),
  by = c("score")
) |>
  full_join(
    score_3_Arbres |>
      group_by(score) |>
      summarize(Diversite_3_Arbres = n()),
    by = c("score")
  ) |>
  full_join(
    score_4_Activites |>
      group_by(score) |>
      summarize(Diversite_4_Activite = n()),
    by = c("score")
  )

## 2 - Synergies

groupesScoresSynergies <- full_join(
  score_1_Integration |>
    group_by(score) |>
    summarize(Synergies_1_Integration = n()),
  score_2_SolPlantes |>
    group_by(score) |>
    summarize(Synergies_2_SolPlantes = n()),
  by = c("score")
) |>
  full_join(
    score_3_IntegrationArbres |>
      group_by(score) |>
      summarize(Synergies_3_IntegrationArbres = n()),
    by = c("score")
  ) |>
  full_join(
    score_4_Connectivite |>
      group_by(score) |>
      summarize(Synergies_4_Connectivite = n()),
    by = c("score")
  )


## 3 - Efficience

groupesScoresEfficience <- full_join(
  score_1_Intrants |>
    group_by(score) |>
    summarize(Efficience_1_Intrants = n()),
  score_2_Engrais |>
    group_by(score) |>
    summarize(Efficience_2_Engrais = n()),
  by = c("score")
) |>
  full_join(
    score_3_Pesticides |>
      group_by(score) |>
      summarize(Efficience_3_Pesticides = n()),
    by = c("score")
  ) |>
  full_join(
    score_4_ProductiviteBesoins |>
      group_by(score) |>
      summarize(Efficience_4_ProductiviteBesoins = n()),
    by = c("score")
  )

# 4 - Recyclage

groupesScoresRecyclage <- full_join(
  score_1_BiomasseDechets |>
    group_by(score) |>
    summarize(Recyclage_1_BiomasseDechets = n()),
  score_2_Eau |>
    group_by(score) |>
    summarize(Recyclage_2_Eau = n()),
  by = c("score")
) |>
  full_join(
    score_3_GrainesRaces |>
      group_by(score) |>
      summarize(Recyclage_3_GrainesRaces = n()),
    by = c("score")
  ) |>
  full_join(
    score_4_Energie |>
      group_by(score) |>
      summarize(Recyclage_4_Energie = n()),
    by = c("score")
  )

## 5 - Résilience

groupesScoresResilience <- full_join(
  score_1_StabiliteProduction |>
    group_by(score) |>
    summarize(Resilience_1_StabiliteProduction = n()),
  score_2_ReductionVulnerabilite |>
    group_by(score) |>
    summarize(Resilience_2_ReductionVulnerabilite = n()),
  by = c("score")
) |>
  arrange(score)

# 6 - Culture et traditions alimentaires

groupesScoresCultureTraditions <- score_1_regimeAlimentaire |>
  group_by(score) |>
  summarize(CultureTraditions_1_regimeAlimentaire = n())

## 7 - Cocréation

groupesScoresCocreation <- full_join(
  score_1_Plateformes |>
    group_by(score) |>
    summarize(Cocreation_1_Plateformes = n()),
  score_2_AccesConnaissances |>
    group_by(score) |>
    summarize(Cocreation_2_AccesConnaissances = n()),
  by = c("score")
) |>
  full_join(
    score_3_Participation |>
      group_by(score) |>
      summarize(Cocreation_3_Participation = n()),
    by = c("score")
  ) |>
  arrange(score)

## 8 - Valeurs humaines et sociales

groupesScoresValeursHumaines <- full_join(
  score_2_Travail |>
    group_by(score) |>
    summarize(ValeursHumaines_2_Travail = n()),
  score_4_BienEtreAnimal |>
    group_by(score) |>
    summarize(ValeursHumaines_4_BienEtreAnimal = n()),
  by = c("score")
) |>
  arrange(score)

# 9 - Economie circulaire

groupesScoresEcoCirculaire <- full_join(
  score_1_MarchesLocaux |>
    group_by(score) |>
    summarize(EcoCirculaire_1_MarchesLocaux = n()),
  score_2_ReseauxProducteurs |>
    group_by(score) |>
    summarize(EcoCirculaire_2_ReseauxProducteurs = n()),
  by = c("score")
) |>
  full_join(
    score_3_SystAlimLocal |>
      group_by(score) |>
      summarize(EcoCirculaire_3_SystAlimLocal = n()),
    by = c("score")
  )

# 10 - Gouvernance

groupesScoresGouvernance <- score_1_Emancipation |>
  group_by(score) |>
  summarize(Gouvernance_1_Emancipation = n())

print(groupesScoresDiversite)
print(groupesScoresSynergies)
print(groupesScoresEfficience)
print(groupesScoresRecyclage)
print(groupesScoresResilience)
print(groupesScoresCultureTraditions)
print(groupesScoresCocreation)
print(groupesScoresValeursHumaines)
print(groupesScoresEcoCirculaire)
print(groupesScoresGouvernance)
