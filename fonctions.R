readCSV <- function(nomFichier, chemin = Sys.getenv("cheminAcces")) {
  readr::read_csv2(file.path(chemin, nomFichier))
}

readTable <- function(nomFichier, dossier, chemin = Sys.getenv("cheminAcces")) {
  readr::read_delim(file.path(chemin, dossier, nomFichier), delim = "\t")
}

recupererFichiers <- function(dossier, chemin = Sys.getenv("cheminAcces")) {
  pathDossier <- file.path(chemin, dossier)
  fichiers <- list.files(pathDossier, pattern = ".tab")
}

parcours <- function(dossier, chemin = Sys.getenv("cheminAcces")) {
  pathDossier <- file.path(chemin, dossier)
  fichiers <- list.files(pathDossier, pattern = ".tab")
  lapply(fichiers, function(x) {
    data <- data.frame(readr::read_delim(file.path(pathDossier, x), delim = "\t"))
    data <- names(data)
  })
}

