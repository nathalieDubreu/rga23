colonnesRga <- readCSV("miseEnForme/colonnesRga23.csv")
colonnesRga |>
  group_by(fichier) |>
  count()

rga23 <- readTable("rga23.tab", dossier)

# Cultures maraîchères....................................10/10
SurfacesCultMarai <- readTable("SurfacesCultMarai.tab", dossier) |>
  rename(
    Culture__id = SurfacesCultMarai__id,
    SurfaceCult = SurfaceCultMarai,
    SurfaceIrrig = SurfaceIrrigCultMarai,
    SurfaceBio = SurfaceBioCultMarai
  ) |>
  mutate(TypeCultures = 10)

# Cultures vivrières......................................20/20
SurfacesCultVivri <- readTable("SurfacesCultVivri.tab", dossier) |>
  rename(
    Culture__id = SurfacesCultVivri__id,
    SurfaceCult = SurfaceCultVivri,
    SurfaceIrrig = SurfaceIrrigCultVivri,
    SurfaceBio = SurfaceBioCultVivri,
    PresenceAutresCult = PresenceAutresSurfVivri
  ) |>
  mutate(TypeCultures = 20)

# Cultures fruitières (hors pépinères) et bois d'oeuvre...30/30
SurfacesCultFruit <- readTable("SurfacesCultFruit.tab", dossier) |>
  rename(
    Culture__id = SurfacesCultFruit__id,
    SurfaceCult = SurfaceCultFruit,
    NbPieds = NbPiedsCultureFruit,
    SurfaceIrrig = SurfaceIrrigCultFruit,
    SurfaceBio = SurfaceBioCultFruit,
    PresenceAutresCult = PresenceAutresSurfFruit
  ) |>
  mutate(TypeCultures = 30)

# Feuillages et cultures florales (hors pépinières).......40/40
SurfacesCultFlorale <- readTable("SurfacesCultFlorale.tab", dossier) |>
  rename(
    Culture__id = SurfacesCultFlorale__id,
    SurfaceCult = SurfaceCultFlorale,
    SurfaceIrrig = SurfaceIrrigCultFlorale,
    SurfaceBio = SurfaceBioCultFlorale
  ) |>
  mutate(TypeCultures = 40)

# Plantes aromatiques, stimulantes et médicinales.........50/50
SurfacesPlantes <- readTable("SurfacesPlantes.tab", dossier) |>
  rename(
    Culture__id = SurfacesPlantes__id,
    SurfaceCult = SurfacePlante,
    SurfaceIrrig = SurfaceIrrigPlante,
    SurfaceBio = SurfaceBioPlante,
    PresenceAutresCult = PresenceAutresSurfPlante
  ) |>
  mutate(TypeCultures = 50)


# Pépinières (plantes vendues en pot).....................60/60
SurfacesPepinieres <- readTable("SurfacesPepinieres.tab", dossier) |>
  rename(
    Culture__id = SurfacesPepinieres__id,
    SurfaceCult = SurfacePepiniere,
    SurfaceIrrig = SurfaceIrrigPepinieres,
    SurfaceBio = SurfaceBioPepinieres
  ) |>
  mutate(TypeCultures = 60)

# Cultures fourragères....................................70/70
SurfacesFourrages <- readTable("SurfacesFourrages.tab", dossier) |>
  rename(
    Culture__id = SurfacesFourrages__id,
    SurfaceCult = SurfaceFourrage,
    SurfaceIrrig = SurfaceIrrigFourrage
  ) |>
  mutate(TypeCultures = 70)

# Jachères................................................80/80
SurfacesJacheres <- readTable("SurfacesJacheres.tab", dossier) |>
  mutate(
    Culture__id = case_when(
      SurfacesJacheres__id == 1 ~ 801,
      SurfacesJacheres__id == 2 ~ 802
    ),
    SurfaceCult = SurfaceJacheres
  ) |>
  select(!SurfacesJacheres__id & !SurfaceJacheres) |>
  mutate(TypeCultures = 80)
