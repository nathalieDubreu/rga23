library(rmarkdown)
library(knitr)

exportRGA <- readTable("rga23.tab", dossier) |>
  filter(interview__key != "59-36-31-34" & interview__key != "06-79-34-97" & interview__key != "26-72-53-00" & interview__key != "49-29-35-86")
iles <- readInputCSV("iles.csv")

rmarkdown::render("suiviCollecte/comptagesRga23.Rmd")
