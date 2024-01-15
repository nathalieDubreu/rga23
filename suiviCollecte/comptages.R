library(rmarkdown)
library(knitr)

exportRGA <- readTable("rga23.tab", dossier) |> filter(interview__key != "59-36-31-34" & interview__key != "06-79-34-97")
iles <- readInputCSV("iles.csv")

rmarkdown::render("suiviCollecte/comptagesRga23.Rmd")
