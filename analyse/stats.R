library(rmarkdown)
library(knitr)

source("analyse/cultivateurs.R")
source("analyse/eleveurs.R")
source("champs/champRGA.R")

rmarkdown::render("analyse/statsRga23.Rmd",encoding="UTF-8")


