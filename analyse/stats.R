library(rmarkdown)
library(knitr)

source("analyse/cultivateurs.R")
source("analyse/eleveurs.R")

rmarkdown::render("analyse/statsRga23.Rmd")


