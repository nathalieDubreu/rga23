library(fmsb)

## Calculs des moyennes par profil

moyenneParProfil <- function(data, sousCategories, variableProfil) {
  resultats <- data |>
    left_join(rga23_profil |> select(interview__key, {{ variableProfil }}), by = "interview__key") |>
    group_by({{ variableProfil }}) |>
    summarize(
      nbExploitations = n(),
      across(all_of(sousCategories),
        .names = "{.col}_{.fn}",
        .fns = list(
          nbExploitations = ~ if (any(is.na(.)) |
            cur_column() == "Efficience_1_Intrants" |
            cur_column() == "Resilience_2_ReductionVulnerabilite" |
            cur_column() == "Gouvernance_1_Emancipation") {
            sum(ifelse(!is.na(.), 1, 0))
          } else {
            NULL
          },
          mean = ~ round(mean(., na.rm = TRUE), 2)
        )
      )
    ) |>
    filter(if_any(everything(), ~ !all(is.na(.))))
  col_names <- gsub("_", " ", names(resultats))
  colnames(resultats) <- col_names
  return(resultats)
}

# Fonctions pour récupérer max et min et les ajouter à la table de résultats pour interprétation dans les graphiques radar

## "vrais" minimum et maximum
# recuperationMin <- function(data) {
#   summarise(
#     data,
#     across(where(is.numeric), ~ min(.))
#   )
# }
# recuperationMax <- function(data) {
#   summarise(
#     data,
#     across(where(is.numeric), ~ max(.))
#   )
# }

## Scores possibles de 0 à 4
recuperationMin <- function(data) {
  data |>
    summarise(
      across(where(is.numeric), ~ 0),
      .groups = "drop"
    )
}
recuperationMax <- function(data) {
  data |>
    summarise(
      across(where(is.numeric), ~ 4),
      .groups = "drop"
    )
}

ajoutMaxMinTable <- function(data, variableProfil) {
  data <- data |>
    select({{ variableProfil }}, ends_with(" mean")) |>
    rename_with(~ gsub(" mean$", "", .), everything())

  max <- recuperationMax(data)
  min <- recuperationMin(data)

  return(rbind(max, min, data |> select(-{{ variableProfil }})))
}
