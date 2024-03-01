rga23_general <- rga23_general |>
  select(
    -SituationConjChefExpl,
    -ActiviteConjoint__1,
    -ActiviteConjoint__10,
    -ActiviteConjoint__11,
    -ActiviteConjoint__12,
    -ActiviteConjoint__2,
    -ActiviteConjoint__3,
    -ActiviteConjoint__4,
    -ActiviteConjoint__5,
    -ActiviteConjoint__6,
    -ActiviteConjoint__7,
    -ActiviteConjoint__8,
    -ActiviteConjoint__9
  )

rga23_gestion <- rga23_gestion|>
  select(
    -Archipel_1,
    -sssys_irnd 
  )

writeCSVTraites(rga23_general)
writeCSVTraites(rga23_gestion)
