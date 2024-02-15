# GESTION DE FICHIERS 

## Lecture

### Lecture de fichiers CSV (en général les CSV exportés de données)
readCSV <- function(nomFichier, chemin = Sys.getenv("cheminAcces")) {
  readr::read_csv2(file.path(chemin, nomFichier), col_types = "c", guess_max = 7832)
}
### Lecture des CSV d'inputs dans le projet
readInputCSV <- function(nomFichier) {
  readr::read_csv2(file.path("input", nomFichier),show_col_types = FALSE)
}
### Lecture du fichier en paramètre (en général les .tab du dossier de données)
readTable <- function(nomFichier, dossier, chemin = Sys.getenv("cheminAcces")) {
  readr::read_delim(file.path(chemin, dossier, nomFichier), delim = "\t", col_types = "c", guess_max = 7832)
}

## Ecriture

### Ecriture dans un dossier SortiesR
writeCSV <- function(table, chemin = Sys.getenv("cheminAcces")) {
  readr::write_csv2(
    table,
    file.path(
      chemin,
      "SortiesR",
      paste(
        deparse(substitute(table)),
        ".csv",
        sep = ""
      )
    ),
    append = FALSE,
    col_names = TRUE
  )
}
### Ecriture à la racine du chemin (A utiliser pour les données traitées)
writeCSVTraites <- function(table, chemin = Sys.getenv("cheminAcces")) {
  readr::write_csv2(
    table,
    file.path(
      chemin,
      paste(
        deparse(substitute(table)),
        ".csv",
        sep = ""
      )
    ),
    append = FALSE,
    col_names = TRUE
  )
}

########################"

# STATISTIQUES

## Calcul des pourcentages 
calculPourcentage <- function(data) {
  data |>
    summarise(count = n()) |>
    mutate(`En %` = round(count / sum(count) * 100, 1)) |>
    select(-count)
}
## Group by sur la colonne groupByColonne + ajout d'une ligne TOTAL et calcul des pourcentages 
groupByTotalEtPourcent <- function(data, groupByColonne, nomColonne) {
  dataGroupBy <- data |>
    group_by({{ groupByColonne }}, {{ nomColonne }}) |>
    calculPourcentage() |>
    spread(key = {{ nomColonne }}, value = `En %`)
  dataTotal <- data |>
    mutate({{ groupByColonne }} := "Total") |>
    group_by({{ groupByColonne }}, {{ nomColonne }}) |>
    calculPourcentage() |>
    spread(key = {{ nomColonne }}, value = `En %`)
  result <- rbind(dataGroupBy, dataTotal)
}
