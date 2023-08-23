eligiblesRGA <- exportRGA |>
  filter((interview__status == 100 | interview__status == 120) & statut_collecte == 1) |>
  filter((eligibiliteCoprah == 1 & substring(id_exploitation, 0, 1) == "C") |
    (eligibilite == 1 & (substring(id_exploitation, 0, 1) == "P" | substring(id_exploitation, 0, 1) == "M")) |
    ((eligibilite == 1 | eligibiliteCoprah == 1) & substring(id_exploitation, 0, 1) == "X"))

eligiblesCultivateurs <- eligiblesRGA |>
  filter(RaisonsRecensement__1 == 1)

cultivateurs <- eligiblesCultivateurs |> summarise(SAUTotale = sum(as.numeric(SurfaceTotalProdAgri)), na.rm=TRUE)

                                                                                     