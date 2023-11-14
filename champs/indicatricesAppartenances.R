source("champs/champRGA.R")
source("champs/champ2012.R")

# Pour le copil : restriction aux questionnaires validés
# rga23 <- rga23 |> filter((interview__status == 130 | interview__status == 120))

# Indicatrices d'appartenance au champ 2023
rga23A <- left_join(rga23, idExploitantsDansLeChamp |> mutate(ValideRGA = 1),
  by = c("interview__key")
)

# Indicatrice d'appartenance au champ 2012
rga23B <- left_join(rga23A, idExploitantsDansLeChamp2012 |> mutate(Valide2012 = 1),
                    by = c("interview__key")
)

rga23B |> group_by(ValideRGA,Valide2012) |> count()

rga23B |> group_by(ElevageValideRGA,ElevageValide2012) |> count()

rga23B |> group_by(CultureValideRGA,CultureValide2012) |> count()

rmarkdown::render("champs/appartenances.Rmd",encoding="UTF-8")
