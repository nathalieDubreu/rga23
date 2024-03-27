Partie1_materielUtilise <- rga23_exploitations |>
  mutate(
    nbCultivateurs = ifelse(RaisonsRecensement__1 == 1, 1, 0),
    UtilMatTraitementRecolte = case_when(
      MaterielTraitRecolte__1 == 1 ~ 1,
      MaterielTraitRecolte__2 == 1 ~ 1,
      MaterielTraitRecolte__3 == 1 ~ 1,
      MaterielTraitRecolte__4 == 1 ~ 1,
      MaterielTraitRecolte__5 == 1 ~ 1,
      MaterielTraitRecolte__6 == 1 ~ 1,
      MaterielTraitRecolte__7 == 1 ~ 1,
      TRUE ~ 0
    ),
    UtilMatProtectionCultures = case_when(
      MatSemisPlant__4 == 1 ~ 1,
      MatSemisPlant__5 == 1 ~ 1,
      MatSemisPlant__6 == 1 ~ 1,
      TRUE ~ 0
    ),
    UtilMatTravauxEntretienSol = case_when(
      MatSemisPlant__1 == 1 ~ 1,
      MatSemisPlant__2 == 1 ~ 1,
      MatSemisPlant__3 == 1 ~ 1,
      MaterielTravailSol__1 == 1 ~ 1,
      MaterielTravailSol__2 == 1 ~ 1,
      MaterielTravailSol__3 == 1 ~ 1,
      MaterielTravailSol__4 == 1 ~ 1,
      MaterielTravailSol__6 == 1 ~ 1,
      MaterielEpandage__1 == 1 ~ 1,
      MaterielEpandage__2 == 1 ~ 1,
      MaterielEpandage__3 == 1 ~ 1,
      MaterielEpandage__5 == 1 ~ 1,
      MaterielEpandage__6 == 1 ~ 1,
      MaterielEpandage__7 == 1 ~ 1,
      MaterielEpandage__8 == 1 ~ 1,
      TRUE ~ 0
    ),
    UtilMatTraction = case_when(
      MaterielTransport__3 == 1 ~ 1,
      MaterielTransport__4 == 1 ~ 1,
      MaterielTransport__5 == 1 ~ 1,
      MaterielTransport__7 == 1 ~ 1,
      MaterielTransport__8 == 1 ~ 1,
      MaterielTransport__9 == 1 ~ 1,
      MaterielTransport__10 == 1 ~ 1,
      TRUE ~ 0
    ),
    UtilMatTransport = case_when(
      MaterielTransport__1 == 1 ~ 1,
      MaterielTransport__2 == 1 ~ 1,
      MaterielTransport__6 == 1 ~ 1,
      MaterielTransport__11 == 1 ~ 1,
      TRUE ~ 0
    )
  ) |>
  summarize(
    nbCultivateurs = sum(nbCultivateurs),
    NbUtilMatTraitementRecolte = sum(UtilMatTraitementRecolte),
    NbUtilMatProtectionCultures = sum(UtilMatProtectionCultures),
    NbUtilMatTravauxEntretienSol = sum(UtilMatTravauxEntretienSol),
    ProportionUtilMatTraitementRecolte = round(sum(UtilMatTraitementRecolte) / nbCultivateurs * 100, 1),
    ProportionUtilMatProtectionCultures = round(sum(UtilMatProtectionCultures) / nbCultivateurs * 100, 1),
    ProportionUtilMatTravauxEntretienSol = round(sum(UtilMatTravauxEntretienSol) / nbCultivateurs * 100, 1),
    nbCultivOuEleveurs = n(),
    NbUtilMatTraction = sum(UtilMatTraction),
    NbUtilMatTransport = sum(UtilMatTransport),
    ProportionUtilMatTraction = round(sum(UtilMatTraction) / nbCultivOuEleveurs * 100, 1),
    ProportionUtilMatTransport = round(sum(UtilMatTransport) / nbCultivOuEleveurs * 100, 1)
  )
writeCSV(Partie1_materielUtilise)
