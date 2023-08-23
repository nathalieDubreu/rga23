library(rmarkdown)
library(knitr)

exportRGA <- readCSV("rga23_08-22.csv")
iles <- readCSV("iles.csv")

date <- "22/08/2023"

rmarkdown::render("suiviCollecte/comptagesRga23.Rmd")
