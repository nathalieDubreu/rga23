rga23 <- readCSV("rga23.csv")

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
