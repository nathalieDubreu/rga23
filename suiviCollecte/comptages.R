library(rmarkdown)
library(knitr)

exportRGA <- readTable("rga23.tab", dossier)
iles <- readCSV("iles.csv")

rmarkdown::render("suiviCollecte/comptagesRga23.Rmd")
