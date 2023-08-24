source("analyse/eligibles.R")

## Eligibles  éleveurs
eligiblesEleveurs <- eligiblesRGA |>
  filter(RaisonsRecensement__2 == 1)

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

