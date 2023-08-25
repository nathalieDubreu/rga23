# Cultures maraîchères....................................10/10
SurfacesCultMarai <- readTable("SurfacesCultMarai.tab", dossier) |>
  mutate(TypeCultures = 10) |>
  rename(
    Culture__id = SurfacesCultMarai__id,
    SurfaceCult = SurfaceCultMarai,
    SurfaceIrrig = SurfaceIrrigCultMarai,
    SurfaceBio = SurfaceBioCultMarai
  )

# Cultures vivrières......................................20/20
SurfacesCultVivri <- readTable("SurfacesCultVivri.tab", dossier) |>
  mutate(TypeCultures = 20) |>
  rename(
    Culture__id = SurfacesCultVivri__id,
    SurfaceCult = SurfaceCultVivri,
    SurfaceIrrig = SurfaceIrrigCultVivri,
    SurfaceBio = SurfaceBioCultVivri,
    PresenceAutresCult = PresenceAutresSurfVivri
  )

# Cultures fruitières (hors pépinères) et bois d'oeuvre...30/30
SurfacesCultFruit <- readTable("SurfacesCultFruit.tab", dossier) |>
  mutate(TypeCultures = 30, NbPiedsCultureFruit = case_when(
    NbPiedsCultureFruit == 9999999 ~ as.numeric(NA),
    TRUE ~ NbPiedsCultureFruit
  )) |>
  rename(
    Culture__id = SurfacesCultFruit__id,
    SurfaceCult = SurfaceCultFruit,
    NbPieds = NbPiedsCultureFruit,
    SurfaceIrrig = SurfaceIrrigCultFruit,
    SurfaceBio = SurfaceBioCultFruit,
    PresenceAutresCult = PresenceAutresSurfFruit
  )

# Feuillages et cultures florales (hors pépinières).......40/40
SurfacesCultFlorale <- readTable("SurfacesCultFlorale.tab", dossier) |>
  mutate(TypeCultures = 40) |>
  rename(
    Culture__id = SurfacesCultFlorale__id,
    SurfaceCult = SurfaceCultFlorale,
    SurfaceIrrig = SurfaceIrrigCultFlorale,
    SurfaceBio = SurfaceBioCultFlorale
  )

# Plantes aromatiques, stimulantes et médicinales.........50/50
SurfacesPlantes <- readTable("SurfacesPlantes.tab", dossier) |>
  mutate(TypeCultures = 50) |>
  rename(
    Culture__id = SurfacesPlantes__id,
    SurfaceCult = SurfacePlante,
    SurfaceIrrig = SurfaceIrrigPlante,
    SurfaceBio = SurfaceBioPlante,
    PresenceAutresCult = PresenceAutresSurfPlante
  )


# Pépinières (plantes vendues en pot).....................60/60
SurfacesPepinieres <- readTable("SurfacesPepinieres.tab", dossier) |>
  mutate(TypeCultures = 60) |>
  rename(
    Culture__id = SurfacesPepinieres__id,
    SurfaceCult = SurfacePepiniere,
    SurfaceIrrig = SurfaceIrrigPepinieres,
    SurfaceBio = SurfaceBioPepinieres
  )

# Cultures fourragères....................................70/70
SurfacesFourrages <- readTable("SurfacesFourrages.tab", dossier) |>
  mutate(TypeCultures = 70) |>
  rename(
    Culture__id = SurfacesFourrages__id,
    SurfaceCult = SurfaceFourrage,
    SurfaceIrrig = SurfaceIrrigFourrage,
    SurfaceBio = SurfaceBioFourrages
  )

# Jachères................................................80/80
SurfacesJacheres <- readTable("SurfacesJacheres.tab", dossier) |>
  mutate(TypeCultures = 80) |>
  mutate(
    Culture__id = case_when(
      SurfacesJacheres__id == 1 ~ 801,
      SurfacesJacheres__id == 2 ~ 802
    ),
    SurfaceCult = SurfaceJacheres
  ) |>
  select(!SurfacesJacheres__id & !SurfaceJacheres)

rga23_surfacesCultures <- bind_rows(
  SurfacesCultFlorale,
  SurfacesCultFruit,
  SurfacesCultMarai,
  SurfacesCultVivri,
  SurfacesJacheres,
  SurfacesPepinieres,
  SurfacesPlantes,
  SurfacesFourrages
)

writeCSVTraites(rga23_surfacesCultures)

rm(
  SurfacesCultFlorale,
  SurfacesCultFruit,
  SurfacesCultMarai,
  SurfacesCultVivri,
  SurfacesJacheres,
  SurfacesPepinieres,
  SurfacesPlantes,
  SurfacesFourrages
)
