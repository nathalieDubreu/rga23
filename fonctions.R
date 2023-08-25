## Lecture

readCSV <- function(nomFichier, chemin = Sys.getenv("cheminAcces")) {
  readr::read_csv2(file.path(chemin, nomFichier))
}

readTable <- function(nomFichier, dossier, chemin = Sys.getenv("cheminAcces")) {
  readr::read_delim(file.path(chemin, dossier, nomFichier), delim = "\t")
}

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
        ".csv"
      )
    ),
    append = FALSE,
    col_names = TRUE
  )
}
