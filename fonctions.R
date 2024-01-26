## Lecture

readCSV <- function(nomFichier, chemin = Sys.getenv("cheminAcces")) {
  readr::read_csv2(file.path(chemin, nomFichier), col_types = "c", guess_max = 7832)
}

## Pour les CSV du projet
readInputCSV <- function(nomFichier) {
  readr::read_csv2(file.path("input", nomFichier),show_col_types = FALSE)
}

readTable <- function(nomFichier, dossier, chemin = Sys.getenv("cheminAcces")) {
  readr::read_delim(file.path(chemin, dossier, nomFichier), delim = "\t", col_types = "c", guess_max = 7832)
}

# readDTA <- function(nomFichier, dossier, chemin = Sys.getenv("cheminAcces")) {
#   read_dta(file.path(chemin, dossier, nomFichier))
# }

### Fichiers tab d'un dossier défini -> liste
lireFichiers <- function(dossier, chemin = Sys.getenv("cheminAcces")) {
  pathDossier <- file.path(chemin, dossier)
  fichiers <- list.files(pathDossier, pattern = ".tab")
  lapply(fichiers, function(x) {
    data <- data.frame(readr::read_delim(file.path(pathDossier, x), delim = "\t"))
    data <- names(data)
  })
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
