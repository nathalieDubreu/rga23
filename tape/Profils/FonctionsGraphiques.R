library(fmsb)

## Calculs des moyennes par profil

moyenneParProfil <- function(data, sousCategories, variableProfil) {
  resultats1 <- data |>
    inner_join(rga23_profil |> select(interview__key, {{ variableProfil }}), by = "interview__key") |>
    group_by({{ variableProfil }}) |>
    summarize(
      Nombre_Exploitations = n(),
      across(all_of(sousCategories),
        .names = "{.col}_{.fn}",
        .fns = list(
          Nb_Exploitations = ~ sum(!is.na(.)),
          Moyenne = ~ round(mean(., na.rm = TRUE), 2)
        )
      )
    ) |>
    filter(if_any(everything(), ~ !all(is.na(.))))

  cols_to_remove <- sapply(resultats1[, -1], function(col) !identical(col, resultats1$Nombre_Exploitations))
  resultats2 <- resultats1[, c(TRUE, cols_to_remove)]

  resultats2 <- cbind(resultats2[, 1], Nombre_Exploitations = resultats$Nombre_Exploitations, resultats2[, -1])

  col_names <- gsub("_", " ", names(resultats2))
  colnames(resultats2) <- col_names
  return(resultats2)
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
      across(where(is.numeric), ~0),
      .groups = "drop"
    )
}
recuperationMax <- function(data) {
  data |>
    summarise(
      across(where(is.numeric), ~4),
      .groups = "drop"
    )
}

ajoutMaxMinTable <- function(data, variableProfil) {
  data <- data |>
    select({{ variableProfil }}, ends_with(" Moyenne")) |>
    rename_with(~ gsub(" Moyenne$", "", .), everything())

  max <- recuperationMax(data)
  min <- recuperationMin(data)

  return(rbind(max, min, data |> select(-{{ variableProfil }})))
}
