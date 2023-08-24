colonnesRga <- readCSV("miseEnForme/colonnesRga23.csv")
colonnesRga |>
  group_by(fichier) |>
  count()

rga23 <- readTable("rga23.tab", dossier)
