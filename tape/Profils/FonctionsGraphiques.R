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
          nbExploitations = ~ if (any(is.na(.))) sum(ifelse(!is.na(.), 1, 0)) else NULL,
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

recuperationMax <- function(data, prefixeCategorie) {
  summarise(
    data,
    across(starts_with(prefixeCategorie), ~ max(.))
  )
}

recuperationMin <- function(data, prefixeCategorie) {
  summarise(
    data,
    across(starts_with(prefixeCategorie), ~ min(.))
  )
}

ajoutMaxMinTable <- function(data, prefixeCategorie, variableProfil) {
  data <- data |>
    select({{ variableProfil }}, ends_with(" mean")) |>
    rename_with(~ gsub(" mean$", "", .), everything())

  max <- recuperationMax(data, prefixeCategorie)
  min <- recuperationMin(data, prefixeCategorie)

  return(rbind(max, min, data |> select(-{{ variableProfil }})))
}
