# Profil des exploitations par archipel (cultures, elevage, coprah)
profilExploitationsParArchipel <- rga23_champ |>
  group_by(Archipel_1) |>
  summarize(
    `Respect seuil Cultures` = sum(indicRGA23_Cultures, na.rm = TRUE),
    `Respect seuil Elevage` = sum(indicRGA23_Elevage, na.rm = TRUE),
    `Respect seuil Coprah` = sum(indicRGA23_Coprah, na.rm = TRUE)
  )

totalExploitationsCultures <- sum(profilExploitationsParArchipel$`Respect seuil Cultures`, na.rm = TRUE)
totalExploitationsElevage <- sum(profilExploitationsParArchipel$`Respect seuil Elevage`, na.rm = TRUE)
totalCoprahValideRGA <- sum(profilExploitationsParArchipel$`Respect seuil Coprah`, na.rm = TRUE)

profilExploitationsParArchipelEtTotal <- profilExploitationsParArchipel |>
  add_row(
    Archipel_1 = "Total",
    `Respect seuil Cultures` = totalExploitationsCultures,
    `Respect seuil Elevage` = totalExploitationsElevage,
    `Respect seuil Coprah` = totalCoprahValideRGA
  )