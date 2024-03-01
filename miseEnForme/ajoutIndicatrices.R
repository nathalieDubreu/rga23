source("champs/champRGA.R")
source("champs/champ2012.R")
source("champs/champCAPL.R")

## Ajout d'une indicatrice dans la table RGA pour les coprahculteurs de plus de 2,7 tonnes (identifiants C et X éligibles)
rga23_gestion <- left_join(rga23_gestion, rga23 |> select(interview__key, eligibiliteCoprah, id_exploitation)) |>
  mutate(indicRGA23_Coprah = case_when(
    (eligibiliteCoprah == 1 & substring(id_exploitation, 0, 1) == "C") ~ 1,
    (eligibiliteCoprah == 1 & substring(id_exploitation, 0, 1) == "X") ~ 1,
    TRUE ~ 0
  )) |>
  select(!eligibiliteCoprah)

# Indicatrices d'appartenance au RGA23 (total, cultures et élevage)
rga23_gestion <- left_join(rga23_gestion, idExploitantsDansLeChamp, by = c("interview__key")) |>
  mutate(
    indicRGA23 = case_when(
      ElevageValideRGA == 1 ~ 1,
      CultureValideRGA == 1 ~ 1,
      indicRGA23_Coprah == 1 ~ 1,
      TRUE ~ 0
    ), indicRGA23_Cultures = case_when(
      CultureValideRGA == 1 ~ 1,
      TRUE ~ 0
    ),
    indicRGA23_Elevage = case_when(
      ElevageValideRGA == 1 ~ 1,
      TRUE ~ 0
    )
  ) |>
  select(!ElevageValideRGA & !CultureValideRGA)

# Indicatrices d'appartenance au RGA12 (total, cultures et élevage)
rga23_gestion <- left_join(rga23_gestion, idExploitantsDansLeChamp2012, by = c("interview__key")) |>
  mutate(
    indicRGA12 = case_when(
      ElevageValide2012  == 1 ~ 1,
      CultureValide2012 == 1 ~ 1,
      TRUE ~ 0
    ), indicRGA12_Cultures = case_when(
      CultureValide2012 == 1 ~ 1,
      TRUE ~ 0
    ),
    indicRGA12_Elevage = case_when(
      ElevageValide2012  == 1 ~ 1,
      TRUE ~ 0
    )
  ) |>
  select(!ElevageValide2012  & !CultureValide2012 & !id_exploitation)

## TODO... indicCAPL

writeCSVTraites(rga23_gestion)
