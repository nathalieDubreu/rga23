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

rm(rga23A, rga23B)

# Fusion par archipel selon les différents seuils

result_2023 <- inner_join(rga23, idExploitantsDansLeChamp, by = c("interview__key")) %>%
  group_by(ArchipelExploitation) %>%
  summarise(`En 2023 avec les seuils de 2023` = n())

result_2012 <- inner_join(rga23, idExploitantsDansLeChamp2012, by = c("interview__key")) %>%
  group_by(ArchipelExploitation) %>%
  summarise(`En 2023 avec les seuils de 2012` = n())

seuilsArchipel <- full_join(result_2012, result_2023, by = "ArchipelExploitation")

# writeCSV(seuilsArchipel)