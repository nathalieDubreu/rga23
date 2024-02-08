## Restriction au champ 12 et 23 du RGA
rga23_champ <- readCSV("rga23_gestion.csv") |>
  filter(indicRGA23 == 1 | indicRGA12 == 1)

surfaces <- inner_join(readCSV("rga23_prodVegetales.csv"), rga23_champ, by = c("interview__key")) |>
  summarize(
    SAUTotale12 = round(sum(ifelse(indicRGA12 == 1, SurfaceTotalProdAgri, 0), na.rm = TRUE) / 10000),
    SurfaceTotaleDeclaree12 = round(sum(ifelse(indicRGA12 == 1, totalSurfDeclarees, 0), na.rm = TRUE) / 10000),
    SAUTotale23 = round(sum(ifelse(indicRGA23 == 1, SurfaceTotalProdAgri, 0), na.rm = TRUE) / 10000),
    SurfaceTotaleDeclaree23 = round(sum(ifelse(indicRGA23 == 1, totalSurfDeclarees, 0), na.rm = TRUE) / 10000)
  )

# Les cultures maraîchères - En 2012, 622 exploitants cultivent 341 ha de produits maraîchers
inner_join(readCSV("rga23_prodVegetales.csv"), rga23_champ, by = c("interview__key")) |>
  summarize(
    nbMaraichage2012 = sum(ifelse(indicRGA12 == 1 & totalSurfaceMarai > 0, 1, 0), na.rm = TRUE),
    maraichage2012 = round(sum(ifelse(indicRGA12 == 1, totalSurfaceMarai, 0), na.rm = TRUE) / 10000),
    nbMaraichage2023 = sum(ifelse(indicRGA23 == 1 & totalSurfaceMarai > 0, 1, 0), na.rm = TRUE),
    maraichage2023 = round(sum(ifelse(indicRGA23 == 1, totalSurfaceMarai, 0), na.rm = TRUE) / 10000)
  )

# Les cultures vivrières - 1 964 exploitations en 2012 - 506 ha
inner_join(readCSV("rga23_prodVegetales.csv"), rga23_champ, by = c("interview__key")) |>
  summarize(
    nbVivrier2012 = sum(ifelse(indicRGA12 == 1 & totalSurfaceVivri > 0, 1, 0), na.rm = TRUE),
    Vivrier2012 = round(sum(ifelse(indicRGA12 == 1, totalSurfaceVivri, 0), na.rm = TRUE) / 10000),
    nbVivrier2023 = sum(ifelse(indicRGA23 == 1 & totalSurfaceVivri > 0, 1, 0), na.rm = TRUE),
    Vivrier2023 = round(sum(ifelse(indicRGA23 == 1, totalSurfaceVivri, 0), na.rm = TRUE) / 10000)
  )

# Les cultures fruitières - 2 343 exploitations en 2012 - 1382 ha
inner_join(readCSV("rga23_prodVegetales.csv"), rga23_champ, by = c("interview__key")) |>
  summarize(
    nbFruitier2012 = sum(ifelse(indicRGA12 == 1 & totalSurfaceFruit > 0, 1, 0), na.rm = TRUE),
    Fruitier2012 = round(sum(ifelse(indicRGA12 == 1, totalSurfaceFruit, 0), na.rm = TRUE) / 10000),
    nbFruitier2023 = sum(ifelse(indicRGA23 == 1 & totalSurfaceFruit > 0, 1, 0), na.rm = TRUE),
    Fruitier2023 = round(sum(ifelse(indicRGA23 == 1, totalSurfaceFruit, 0), na.rm = TRUE) / 10000)
  )

## Hors cocoteraies
inner_join(readCSV("rga23_surfacesCultures.csv"), rga23_champ, by = c("interview__key")) |>
  filter(TypeCulture == 30 & culture_id != 307 & culture_id != 308 & culture_id != 309) |>
  summarize(
    Fruitier2012HorsCoco = round(sum(ifelse(indicRGA12 == 1, SurfaceCult, 0), na.rm = TRUE) / 10000),
    Fruitier2023HorsCoco = round(sum(ifelse(indicRGA23 == 1, SurfaceCult, 0), na.rm = TRUE) / 10000)
  )

# Les cultures florales - En 2012, 701 exploitations se répartissent 156 ha
inner_join(readCSV("rga23_prodVegetales.csv"), rga23_champ, by = c("interview__key")) |>
  summarize(
    nbFlorales2012 = sum(ifelse(indicRGA12 == 1 & totalSurfaceFlorale > 0, 1, 0), na.rm = TRUE),
    Florales2012 = round(sum(ifelse(indicRGA12 == 1, totalSurfaceFlorale, 0), na.rm = TRUE) / 10000),
    nbFlorales2023 = sum(ifelse(indicRGA23 == 1 & totalSurfaceFlorale > 0, 1, 0), na.rm = TRUE),
    Florales2023 = round(sum(ifelse(indicRGA23 == 1, totalSurfaceFlorale, 0), na.rm = TRUE) / 10000)
  )

# Les cultures aromatiques - 1 101 exploitations en 2012, avec une SAU de 313 ha
inner_join(readCSV("rga23_prodVegetales.csv"), rga23_champ, by = c("interview__key")) |>
  summarize(
    nbPlantes2012 = sum(ifelse(indicRGA12 == 1 & totalSurfacePlantes > 0, 1, 0), na.rm = TRUE),
    Plantes2012 = round(sum(ifelse(indicRGA12 == 1, totalSurfacePlantes, 0), na.rm = TRUE) / 10000),
    nbPlantes2023 = sum(ifelse(indicRGA23 == 1 & totalSurfacePlantes > 0, 1, 0), na.rm = TRUE),
    Plantes2023 = round(sum(ifelse(indicRGA23 == 1, totalSurfacePlantes, 0), na.rm = TRUE) / 10000)
  )

# Bovins........................1/1
# Ovins.........................2/2
# Caprins.......................8/8
# Porcins.......................3/3
# Volailles.....................4/4
# Equidés.......................5/5
# Lapins élevés pour la chair...6/6
# Abeilles......................7/7

# L’apiculture - 2012 : 70 apiculteurs et 2 960 ruches
inner_join(readCSV("rga23_prodAnimales.csv"), rga23_champ, by = c("interview__key")) |>
  filter(PresenceAnimaux__7 == 1) |>
  summarize(
    nbEleveurs2012 = sum(ifelse(indicRGA12 == 1, 1, 0), na.rm = TRUE),
    Animaux2012 = sum(ifelse(indicRGA12 == 1, NbRuchesPourProduire, 0), na.rm = TRUE),
    nbEleveurs2023 = sum(ifelse(indicRGA23 == 1, 1, 0), na.rm = TRUE),
    Animaux2023 = sum(ifelse(indicRGA23 == 1, NbRuchesPourProduire, 0), na.rm = TRUE)
  )

# L’élevage porcin - En 2012, 283 exploitants élèvent 13 617 porcs
inner_join(readCSV("rga23_prodAnimales.csv"), rga23_champ, by = c("interview__key")) |>
  filter(PresenceAnimaux__3 == 1) |>
  summarize(
    nbEleveurs2012 = sum(ifelse(indicRGA12 == 1, 1, 0), na.rm = TRUE),
    Animaux2012 = sum(ifelse(indicRGA12 == 1, nbTotalPorcs, 0), na.rm = TRUE),
    nbEleveurs2023 = sum(ifelse(indicRGA23 == 1, 1, 0), na.rm = TRUE),
    Animaux2023 = sum(ifelse(indicRGA23 == 1, nbTotalPorcs, 0), na.rm = TRUE)
  )

# L’élevage bovin - En 2012, 125 exploitations élèvent 4 670 bovins.
inner_join(readCSV("rga23_prodAnimales.csv"), rga23_champ, by = c("interview__key")) |>
  filter(PresenceAnimaux__1 == 1) |>
  summarize(
    nbEleveurs2012 = sum(ifelse(indicRGA12 == 1, 1, 0), na.rm = TRUE),
    Animaux2012 = sum(ifelse(indicRGA12 == 1, nbTotalBovins, 0), na.rm = TRUE),
    nbEleveurs2023 = sum(ifelse(indicRGA23 == 1, 1, 0), na.rm = TRUE),
    Animaux2023 = sum(ifelse(indicRGA23 == 1, nbTotalBovins, 0), na.rm = TRUE)
  )

# L’élevage de caprins - En 2012, 165 exploitations élèvent 9 301 bêtes
inner_join(readCSV("rga23_prodAnimales.csv"), rga23_champ, by = c("interview__key")) |>
  filter(PresenceAnimaux__8 == 1) |>
  summarize(
    nbEleveurs2012 = sum(ifelse(indicRGA12 == 1, 1, 0), na.rm = TRUE),
    Animaux2012 = sum(ifelse(indicRGA12 == 1, nbTotalCaprins, 0), na.rm = TRUE),
    nbEleveurs2023 = sum(ifelse(indicRGA23 == 1, 1, 0), na.rm = TRUE),
    Animaux2023 = sum(ifelse(indicRGA23 == 1, nbTotalCaprins, 0), na.rm = TRUE)
  )

# L’élevage de volailles - En 2012, 210 257 volailles sont élevées dans 55 exploitations
inner_join(readCSV("rga23_prodAnimales.csv"), rga23_champ, by = c("interview__key")) |>
  filter(PresenceAnimaux__4 == 1) |>
  mutate(NombreVolailles = rowSums(across(
    c("NbOies", "NbCanards", "NbPintades", "NbPouletsChairCoqs", "NbPoulettes", "NbPoussins", "NombrePoules0", "NombrePoules1", "NombrePoules3"),
    ~ coalesce(.x, 0)
  ))) |>
  summarize(
    nbEleveurs2012 = sum(ifelse(indicRGA12 == 1, 1, 0), na.rm = TRUE),
    Animaux2012 = sum(ifelse(indicRGA12 == 1, NombreVolailles, 0), na.rm = TRUE),
    nbEleveurs2023 = sum(ifelse(indicRGA23 == 1, 1, 0), na.rm = TRUE),
    Animaux2023 = sum(ifelse(indicRGA23 == 1, NombreVolailles, 0), na.rm = TRUE)
  )
