# Décision 25/03/2024: Ajout points CAPL et indicatrice CAPL
source("champs/champCAPL.R")
rga23_general <- left_join(rga23_general,
  idExploitantsPointsCAPL,
  by = c("interview__key", "Archipel_1", "indicRGA23_Coprah")
) |>
  mutate(
    indicPointsCAPL = case_when(
      PointsCAPL >= 400 ~ 1,
      indicRGA23_Coprah == 1 ~ 1,
      TRUE ~ 0
    )
  )

writeCSVTraites(rga23_general)
