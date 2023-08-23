eligiblesRGA <- exportRGA |>
  filter((interview__status == 100 | interview__status == 120) & statut_collecte == 1) |>
  filter((eligibiliteCoprah == 1 & substring(id_exploitation, 0, 1) == "C") |
    (eligibilite == 1 & (substring(id_exploitation, 0, 1) == "P" | substring(id_exploitation, 0, 1) == "M")) |
    ((eligibilite == 1 | eligibiliteCoprah == 1) & substring(id_exploitation, 0, 1) == "X"))

## Cultivateurs

eligiblesCultivateurs <- eligiblesRGA |>
  filter(RaisonsRecensement__1 == 1)

cultivateurs <- eligiblesCultivateurs |>
  mutate(SAU = case_when(is.na(SurfaceTotalProdAgri) ~ 0, TRUE ~ as.numeric(SurfaceTotalProdAgri))) |>
  summarise(SAU_totale_hectare = sum(SAU)/10000, SAU_moyenne_hectare = mean(SAU)/10000)


## Cultivateurs

eligiblesEleveurs <- eligiblesRGA |>
  filter(RaisonsRecensement__2 == 1)


# Bovins........................1/1
# Ovins.........................2/2
# Caprins.......................8/8
# Porcins.......................3/3
# Volailles.....................4/4
# Equidés.......................5/5
# Lapins élevés pour la chair...6/6
# Abeilles......................7/7

eleveursBovins <- eligiblesEleveurs |>
  filter(PresenceAnimaux__1 == 1)
