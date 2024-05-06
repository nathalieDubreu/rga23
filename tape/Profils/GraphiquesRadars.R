library(rmarkdown)
library(knitr)
library(fmsb)

## Calculs des moyennes par archipel

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
          mean = ~ round(mean(., na.rm = TRUE), 2)
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

df <- rbind(max, min, moyennesDiversiteParArchipel) |>
  rename_with(~ gsub("_mean$", "", .), everything())

diversiteSansArchipel <- df |> select(-Archipel_1)

# SYNERGIE

moyennesSynergiesParArchipel <- resultatsSynergiesParArchipel |>
  select(Archipel_1, ends_with("_mean"))

max <- moyennesSynergiesParArchipel |> recuperationMax(prefixeCategorie = "Synergies_")
min <- moyennesSynergiesParArchipel |> recuperationMin(prefixeCategorie = "Synergies_")

df <- rbind(max, min, moyennesSynergiesParArchipel) |>
  rename_with(~ gsub("_mean$", "", .), everything())

synergieSansArchipel <- df |> select(-Archipel_1)

rmarkdown::render("tape/Profils/graphiquesRadars.Rmd")
