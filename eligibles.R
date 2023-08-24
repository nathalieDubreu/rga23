eligiblesRGA <- exportRGA |>
  filter((interview__status == 100 | interview__status == 120) & statut_collecte == 1) |>
  filter((eligibiliteCoprah == 1 & substring(id_exploitation, 0, 1) == "C") |
    (eligibilite == 1 & (substring(id_exploitation, 0, 1) == "P" | substring(id_exploitation, 0, 1) == "M")) |
    ((eligibilite == 1 | eligibiliteCoprah == 1) & substring(id_exploitation, 0, 1) == "X"))

## Eligibles cultivateurs / éleveurs
eligiblesCultivateurs <- eligiblesRGA |>
  filter(RaisonsRecensement__1 == 1)

eligiblesEleveurs <- eligiblesRGA |>
  filter(RaisonsRecensement__2 == 1)

## QQ surfaces
cultivateurs <- eligiblesCultivateurs |>
  mutate(SAU = case_when(is.na(SurfaceTotalProdAgri) ~ 0, TRUE ~ as.numeric(SurfaceTotalProdAgri))) |>
  summarise(
    SAU_totale_hectare = sum(SAU) / 10000,
    SAU_moyenne_hectare = mean(SAU) / 10000,
    SAU_max_hectare = max(SAU) / 10000
  )

### Par type de cultures classiques ou d'animaux

# Cultures maraîchères....................................10/10
# Cultures vivrières......................................20/20
# Cultures fruitières (hors pépinères) et bois d'oeuvre...30/30
# Feuillages et cultures florales (hors pépinières).......40/40
# Plantes aromatiques, stimulantes et médicinales.........50/50
# Pépinières (plantes vendues en pot).....................60/60
# Cultures fourragères....................................70/70
# Jachères................................................80/80
eligiblesCultivateurs |>
  filter(ModesProduction__1 == 1) |>
  summarise(
    cultMaraicheres = sum(CulturesPresentes__10),
    cultVivrieres = sum(CulturesPresentes__20),
    cultFruitieres = sum(CulturesPresentes__30),
    cultFlorales = sum(CulturesPresentes__40),
    cultPPAM = sum(CulturesPresentes__50),
    cultPepinieres = sum(CulturesPresentes__60),
    cultFourrageres = sum(CulturesPresentes__70),
    cultJacheres = sum(CulturesPresentes__80)
  )

# Bovins........................1/1
# Ovins.........................2/2
# Porcins.......................3/3
# Volailles.....................4/4
# Equidés.......................5/5
# Lapins élevés pour la chair...6/6
# Abeilles......................7/7
# Caprins.......................8/8
eligiblesEleveurs |>
  summarise(
    elevBovins = sum(PresenceAnimaux__1),
    elevOvins = sum(PresenceAnimaux__2),
    elevPorcins = sum(PresenceAnimaux__3),
    elevVolailles = sum(PresenceAnimaux__4),
    elevEquides = sum(PresenceAnimaux__5),
    elevLapins = sum(PresenceAnimaux__6),
    elevAbeilles = sum(PresenceAnimaux__7),
    elevCaprins = sum(PresenceAnimaux__8)
  )

## TODO : A vérifier éleveurs sans paturage (pour bovins, caprins, ovins et équidés a minima)
aVerifier <- eligiblesEleveurs |>
  filter((PresenceAnimaux__1 == 1 | PresenceAnimaux__2 == 1 | PresenceAnimaux__5 == 1 | PresenceAnimaux__8 == 1) &
    RaisonsRecensement__1 == 0)
