# Bilan des classements

## 1 - Diversité

scoresDiversite <- full_join(
  score_1_Cultures |>
    ungroup() |>
    select(interview__key, score) |>
    mutate(Diversite_1_Culture = case_when(score <= 4 ~ score, TRUE ~ NA)) |> select(-score),
  score_2_Animaux |>
    select(interview__key, score) |>
    mutate(Diversite_2_Animaux = case_when(score <= 4 ~ score, TRUE ~ NA)) |> select(-score),
  by = "interview__key"
) |>
  full_join(
    score_3_Arbres |>
      select(interview__key, score) |>
      mutate(Diversite_3_Arbres = case_when(score <= 4 ~ score, TRUE ~ NA)) |> select(-score),
    by = "interview__key"
  ) |>
  full_join(
    score_4_Activites |>
      select(interview__key, score) |>
      mutate(Diversite_4_Activite = case_when(score <= 4 ~ score, TRUE ~ NA)) |> select(-score),
    by = "interview__key"
  )

## 2 - Synergies

scoresSynergies <- full_join(
  score_1_Integration |>
    select(interview__key, score) |>
    mutate(Synergies_1_Integration = case_when(score <= 4 ~ score, TRUE ~ NA)) |> select(-score),
  score_2_SolPlantes |>
    select(interview__key, score) |>
    mutate(Synergies_2_SolPlantes = case_when(score <= 4 ~ score, TRUE ~ NA)) |> select(-score),
  by = "interview__key"
) |>
  full_join(
    score_3_IntegrationArbres |>
      select(interview__key, score) |>
      mutate(Synergies_3_IntegrationArbres = case_when(score <= 4 ~ score, TRUE ~ NA)) |> select(-score),
    by = "interview__key"
  ) |>
  full_join(
    score_4_Connectivite |>
      select(interview__key, score) |>
      mutate(Synergies_4_Connectivite = case_when(score <= 4 ~ score, TRUE ~ NA)) |> select(-score),
    by = "interview__key"
  )


## 3 - Efficience

scoresEfficience <- full_join(
  score_1_Intrants |>
    select(interview__key, score) |>
    mutate(Efficience_1_Intrants = case_when(score <= 4 ~ score, TRUE ~ NA)) |> select(-score),
  score_2_Engrais |>
    select(interview__key, score) |>
    mutate(Efficience_2_Engrais = case_when(score <= 4 ~ score, TRUE ~ NA)) |> select(-score),
  by = "interview__key"
) |>
  full_join(
    score_3_Pesticides |>
      select(interview__key, score) |>
      mutate(Efficience_3_Pesticides = case_when(score <= 4 ~ score, TRUE ~ NA)) |> select(-score),
    by = "interview__key"
  ) |>
  full_join(
    score_4_ProductiviteBesoins |>
      select(interview__key, score) |>
      mutate(Efficience_4_ProductiviteBesoins = case_when(score <= 4 ~ score, TRUE ~ NA)) |> select(-score),
    by = "interview__key"
  )

# 4 - Recyclage

scoresRecyclage <- full_join(
  score_1_BiomasseDechets |>
    select(interview__key, score) |>
    mutate(Recyclage_1_BiomasseDechets = case_when(score <= 4 ~ score, TRUE ~ NA)) |> select(-score),
  score_2_Eau |>
    select(interview__key, score) |>
    mutate(Recyclage_2_Eau = case_when(score <= 4 ~ score, TRUE ~ NA)) |> select(-score),
  by = "interview__key"
) |>
  full_join(
    score_3_GrainesRaces |>
      select(interview__key, score) |>
      mutate(Recyclage_3_GrainesRaces = case_when(score <= 4 ~ score, TRUE ~ NA)) |> select(-score),
    by = "interview__key"
  ) |>
  full_join(
    score_4_Energie |>
      select(interview__key, score) |>
      mutate(Recyclage_4_Energie = case_when(score <= 4 ~ score, TRUE ~ NA)) |> select(-score),
    by = "interview__key"
  )

# 6 - Culture et traditions alimentaires

scoresCultureTraditions <- score_1_regimeAlimentaire |>
  select(interview__key, score) |>
  mutate(CultureTraditions_1_regimeAlimentaire = case_when(score <= 4 ~ score, TRUE ~ NA)) |>
  select(-score)

## 7 - Cocréation

scoresCocreation <- full_join(
  score_1_Plateformes |>
    select(interview__key, score) |>
    mutate(Cocreation_1_Plateformes = case_when(score <= 4 ~ score, TRUE ~ NA)) |>
    select(-score),
  score_2_AccesConnaissances |>
    select(interview__key, score) |>
    mutate(Cocreation_2_AccesConnaissances = case_when(score <= 4 ~ score, TRUE ~ NA)) |>
    select(-score),
  by = "interview__key"
) |>
  full_join(
    score_3_Participation |>
      select(interview__key, score) |>
      mutate(Cocreation_3_Participation = case_when(score <= 4 ~ score, TRUE ~ NA)) |>
      select(-score),
    by = "interview__key"
  )

# 9 - Economie circulaire

scoresEcoCirculaire <- score_1_MarchesLocaux |>
  select(interview__key, score) |>
  mutate(EcoCirculaire_1_MarchesLocaux = case_when(score <= 4 ~ score, TRUE ~ NA)) |>
  select(-score)

# 10 - Gouvernance

scoresGouvernance <- score_1_Emancipation |>
  select(interview__key, score) |>
  mutate(Gouvernance_1_Emancipation = case_when(score <= 4 ~ score, TRUE ~ NA)) |>
  select(-score)




moyenneParArchipel <- function(data, categories) {
  resultats <- data |>
    left_join(rga23_general |> select(interview__key, Archipel_1), by = "interview__key") |>
    group_by(Archipel_1) |>
    summarize(
      nbExploitations = n(),
      across(all_of(categories),
        .names = "{.col}_{.fn}",
        .fns = list(
          nbExploitations = ~ if (any(is.na(.))) sum(ifelse(!is.na(.), 1, 0)) else NULL,
          mean = ~ mean(., na.rm = TRUE)
        )
      )
    ) |>
    filter(if_any(everything(), ~ !all(is.na(.))))
  return(resultats)
}

resultatsDiversiteParArchipel <- moyenneParArchipel(scoresDiversite, c("Diversite_1_Culture", "Diversite_2_Animaux", "Diversite_3_Arbres", "Diversite_4_Activite"))
resultatsSynergiesParArchipel <- moyenneParArchipel(scoresSynergies, c("Synergies_1_Integration", "Synergies_2_SolPlantes", "Synergies_3_IntegrationArbres", "Synergies_4_Connectivite"))

# Fonctions pour récupérer max et min

recuperationMax <- function(data, prefixeCategorie) {
  summarise(
    data,
    Archipel_1 = "Max",
    across(starts_with(prefixeCategorie), ~ max(.))
  )
}

recuperationMin <- function(data, prefixeCategorie) {
  summarise(
    data,
    Archipel_1 = "Min",
    across(starts_with(prefixeCategorie), ~ min(.))
  )
}
# DIVERSITE

moyennesDiversiteParArchipel <- resultatsDiversiteParArchipel |>
  select(Archipel_1, ends_with("_mean"))

max <- moyennesDiversiteParArchipel |> recuperationMax(prefixeCategorie = "Diversite_")
min <- moyennesDiversiteParArchipel |> recuperationMin(prefixeCategorie = "Diversite_")

df <- rbind(max , min, moyennesDiversiteParArchipel) |>
  rename_with(~gsub("_mean$", "", .), everything())

diversiteSansArchipel <- df |> select(-Archipel_1)

# SYNERGIE

moyennesSynergiesParArchipel <- resultatsSynergiesParArchipel |>
  select(Archipel_1, ends_with("_mean"))

max <- moyennesSynergiesParArchipel |> recuperationMax(prefixeCategorie = "Synergies_")
min <- moyennesSynergiesParArchipel |> recuperationMin(prefixeCategorie = "Synergies_")
  
df <- rbind(max , min, moyennesSynergiesParArchipel) |>
  rename_with(~gsub("_mean$", "", .), everything())

synergieSansArchipel <- df |> select(-Archipel_1)


library(rmarkdown)
library(knitr)
library(fmsb)

rmarkdown::render("tape/graphiquesRadars.Rmd")
