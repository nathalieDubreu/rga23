readCSV <- function(nomFichier, chemin = Sys.getenv("cheminAcces")) {
  readr::read_csv2(file.path(chemin,nomFichier))
}
 
