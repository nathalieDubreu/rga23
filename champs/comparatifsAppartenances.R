source("champs/champRGA.R")
source("champs/champ2012.R")
source("champs/champCAPL.R")

## Hors ceux qui sont valides via le coprah
idExploitantsPointsCAPL_HC <- idExploitantsPointsCAPL |> filter(PointsCAPL >= 400)

# Pour le copil : restriction aux questionnaires validés
# rga23 <- rga23 |> filter((interview__status == 130 | interview__status == 120))

# Indicatrices d'appartenance au champ 2023
rga23 <- left_join(readCSV("rga23_general.csv"), idExploitantsDansLeChamp |> mutate(ValideRGA = 1),
  by = c("interview__key")
) |>
  left_join(
    readCSV("rga23_coprahculteurs.csv") |> select(interview__key, eligibiliteCoprah),
    by = c("interview__key"),
  ) |>
  left_join(
    readCSV("rga23_exploitations.csv") |> select(interview__key, eligibilite),
    by = c("interview__key"),
  )

# Indicatrice d'appartenance au champ 2012
rga23A <- left_join(rga23, idExploitantsDansLeChamp2012 |> mutate(Valide2012 = 1),
  by = c("interview__key")
)

# Indicatrice de respect des seuils CAPL
rga23B <- left_join(rga23A, idExploitantsPointsCAPL_HC |> mutate(ValideCAPL_HC = 1) |> select(-Archipel_1, -indicRGA23_Coprah),
  by = c("interview__key")
)

rga23B |>
  group_by(ValideRGA, Valide2012, ValideCAPL_HC) |>
  count()

rga23B |>
  group_by(ElevageValideRGA, ElevageValide2012) |>
  count()

rga23B |>
  group_by(CultureValideRGA, CultureValide2012) |>
  count()

rmarkdown::render("champs/appartenances.Rmd", encoding = "UTF-8")

# Fusion par archipel selon les différents seuils - Nombre d'exploitations en 2023

result_2023 <- rga23B |>
  filter(ValideRGA == 1) |>
  group_by(Archipel_1) |>
  summarise(`Seuils de 2023` = n())
total <- sum(result_2023$`Seuils de 2023`)
result_2023 <- result_2023 |>
  add_row(Archipel_1 = "Total", `Seuils de 2023` = total)

result_2012 <- rga23B |>
  filter(Valide2012 == 1) |>
  group_by(Archipel_1) |>
  summarise(`Seuils de 2012` = n())
total <- sum(result_2012$`Seuils de 2012`)
result_2012 <- result_2012 |>
  add_row(Archipel_1 = "Total", `Seuils de 2012` = total)

result_CAPL <- rga23B |>
  filter(ValideCAPL_HC == 1) |>
  group_by(Archipel_1) |>
  summarise(`Seuils de la CAPL` = n())
total <- sum(result_CAPL$`Seuils de la CAPL`)
result_CAPL <- result_CAPL |>
  add_row(Archipel_1 = "Total", `Seuils de la CAPL` = total)

seuilsArchipel <- full_join(result_2012, result_2023, by = "Archipel_1") |>
  full_join(result_CAPL, by = "Archipel_1")

writeCSV(seuilsArchipel)

rm(rga23, rga23A)
