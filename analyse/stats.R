library(rmarkdown)
library(knitr)

source("champs/champRGA.R")

rmarkdown::render("analyse/statsRga23.Rmd",encoding="UTF-8")

# rga23_champ |> group_by(CultureValideRGA, ElevageValideRGA, CoprahValideRGA) |> count()

