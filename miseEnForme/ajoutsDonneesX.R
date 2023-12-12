## Récupération des données relatives aux X obtenues par téléphone 
### (hors variables spécifiques au roster MO permanente non familiale - cf. rostersMainOeuvre.R)

# WIP

donneesMO <- readInputCSV("donneesMainOeuvre.csv") |> select(!id_exploitation)

rga23AvecDonneesX <- left_join(rga23, donneesMO, by=c("interview__key"), suffix = c("_rga23", "_donneesMO"))

# Liste des variables communes à traiter
variables_comunes <- setdiff(intersect(names(rga23), names(donneesMO)), "interview__key")

# Appliquer la condition pour chaque variable commune
for (variable in variables_comunes) {
  condition <- paste0(variable, " = ifelse(!is.na(", variable, "_donneesMO), ", variable, "_donneesMO, ", variable, "_rga23)")
  rga23AvecDonneesX <- rga23AvecDonneesX |> mutate(!!variable := eval(parse(text = condition)))
}

rga23 <- rga23AvecDonneesX |> select(-ends_with("_rga23")) |> select(-ends_with("_donneesMO"))

rm(
  donneesMO,
  rga23AvecDonneesX
)

writeCSVTraites(rga23)
