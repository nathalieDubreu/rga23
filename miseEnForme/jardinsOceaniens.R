source("analyse/eligibles.R")

# Jardins océaniens

contenuJardinsOceaniens <- rga23 |>
  filter(ModesProduction__4 == 1) |>
  mutate(nbTypesCultures = CultPresentesJardins__10 +
    CultPresentesJardins__20 +
    CultPresentesJardins__30 +
    CultPresentesJardins__40 +
    CultPresentesJardins__50 +
    CultPresentesJardins__60 +
    CultPresentesJardins__70 +
    CultPresentesJardins__80) |>
  select(
    interview__key,
    interview__status,
    id_enqueteur_ech,
    SurfaceJardins,
    nbTypesCultures,
    totalSurfaceFruit,
    totalSurfaceMarai,
    totalSurfaceVivri,
    totalSurfaceFlorale,
    totalSurfacePlantes,
    CultPresentesJardins__10,
    CultPresentesJardins__20,
    CultPresentesJardins__30,
    CultPresentesJardins__40,
    CultPresentesJardins__50,
    CultPresentesJardins__60,
    CultPresentesJardins__70,
    CultPresentesJardins__80,
    CultPrincipJardins__0,
    CultPrincipJardins__1,
    CultPrincipJardins__2,
    CultPrincipJardins__3,
    CultPrincipJardins__4
  )

# Cultures maraîchères....................................10/10
# Cultures vivrières......................................20/20
# Cultures fruitières (hors pépinères) et bois d'oeuvre...30/30
# Feuillages et cultures florales (hors pépinières).......40/40
# Plantes aromatiques, stimulantes et médicinales.........50/50
# Pépinières (plantes vendues en pot).....................60/60
# Cultures fourragères....................................70/70
# Jachères................................................80/80

contenuJardinsOceaniens |>
  filter(CultPresentesJardins__10 == 0 & (substring(CultPrincipJardins__0, 0, 1) == 1 |
    substring(CultPrincipJardins__1, 0, 1) == 1 |
    substring(CultPrincipJardins__2, 0, 1) == 1 |
    substring(CultPrincipJardins__3, 0, 1) == 1 |
    substring(CultPrincipJardins__4, 0, 1) == 1)) |>
  count()
# 32

contenuJardinsOceaniens |>
  filter(CultPresentesJardins__20 == 0 & (substring(CultPrincipJardins__0, 0, 1) == 2 |
    substring(CultPrincipJardins__1, 0, 1) == 2 |
    substring(CultPrincipJardins__2, 0, 1) == 2 |
    substring(CultPrincipJardins__3, 0, 1) == 2 |
    substring(CultPrincipJardins__4, 0, 1) == 2)) |>
  count()
# 21

contenuJardinsOceaniens |>
  filter(CultPresentesJardins__30 == 0 & (substring(CultPrincipJardins__0, 0, 1) == 3 |
    substring(CultPrincipJardins__1, 0, 1) == 3 |
    substring(CultPrincipJardins__2, 0, 1) == 3 |
    substring(CultPrincipJardins__3, 0, 1) == 3 |
    substring(CultPrincipJardins__4, 0, 1) == 3)) |>
  count()

# 9

contenuJardinsOceaniens |>
  filter(CultPresentesJardins__40 == 0 & (substring(CultPrincipJardins__0, 0, 1) == 4 |
    substring(CultPrincipJardins__1, 0, 1) == 4 |
    substring(CultPrincipJardins__2, 0, 1) == 4 |
    substring(CultPrincipJardins__3, 0, 1) == 4 |
    substring(CultPrincipJardins__4, 0, 1) == 4)) |>
  count()
# 2

contenuJardinsOceaniens |>
  filter(CultPresentesJardins__50 == 0 & (substring(CultPrincipJardins__0, 0, 1) == 5 |
    substring(CultPrincipJardins__1, 0, 1) == 5 |
    substring(CultPrincipJardins__2, 0, 1) == 5 |
    substring(CultPrincipJardins__3, 0, 1) == 5 |
    substring(CultPrincipJardins__4, 0, 1) == 5)) |>
  count()
# 12

contenuJardinsOceaniens |>
  filter(CultPresentesJardins__60 == 0 & (substring(CultPrincipJardins__0, 0, 1) == 6 |
    substring(CultPrincipJardins__1, 0, 1) == 6 |
    substring(CultPrincipJardins__2, 0, 1) == 6 |
    substring(CultPrincipJardins__3, 0, 1) == 6 |
    substring(CultPrincipJardins__4, 0, 1) == 6)) |>
  count()
# 11

contenuJardinsOceaniens |>
  filter(CultPresentesJardins__70 == 0 & (substring(CultPrincipJardins__0, 0, 1) == 7 |
    substring(CultPrincipJardins__1, 0, 1) == 7 |
    substring(CultPrincipJardins__2, 0, 1) == 7 |
    substring(CultPrincipJardins__3, 0, 1) == 7 |
    substring(CultPrincipJardins__4, 0, 1) == 7)) |>
  count()
# 0

contenuJardinsOceaniens |>
  filter(CultPresentesJardins__80 == 0 & (substring(CultPrincipJardins__0, 0, 1) == 8 |
    substring(CultPrincipJardins__1, 0, 1) == 8 |
    substring(CultPrincipJardins__2, 0, 1) == 8 |
    substring(CultPrincipJardins__3, 0, 1) == 8 |
    substring(CultPrincipJardins__4, 0, 1) == 8)) |>
  count()
# 0

############

contenuJardinsOceaniens |>
  filter((CultPresentesJardins__10 == 0 & (substring(CultPrincipJardins__0, 0, 1) == 1 |
    substring(CultPrincipJardins__1, 0, 1) == 1 |
    substring(CultPrincipJardins__2, 0, 1) == 1 |
    substring(CultPrincipJardins__3, 0, 1) == 1 |
    substring(CultPrincipJardins__4, 0, 1) == 1)) |
    (CultPresentesJardins__20 == 0 & (substring(CultPrincipJardins__0, 0, 1) == 2 |
      substring(CultPrincipJardins__1, 0, 1) == 2 |
      substring(CultPrincipJardins__2, 0, 1) == 2 |
      substring(CultPrincipJardins__3, 0, 1) == 2 |
      substring(CultPrincipJardins__4, 0, 1) == 2)) |
    (CultPresentesJardins__30 == 0 & (substring(CultPrincipJardins__0, 0, 1) == 3 |
      substring(CultPrincipJardins__1, 0, 1) == 3 |
      substring(CultPrincipJardins__2, 0, 1) == 3 |
      substring(CultPrincipJardins__3, 0, 1) == 3 |
      substring(CultPrincipJardins__4, 0, 1) == 3)) |
    (CultPresentesJardins__40 == 0 & (substring(CultPrincipJardins__0, 0, 1) == 4 |
      substring(CultPrincipJardins__1, 0, 1) == 4 |
      substring(CultPrincipJardins__2, 0, 1) == 4 |
      substring(CultPrincipJardins__3, 0, 1) == 4 |
      substring(CultPrincipJardins__4, 0, 1) == 4)) |
    (CultPresentesJardins__50 == 0 & (substring(CultPrincipJardins__0, 0, 1) == 5 |
      substring(CultPrincipJardins__1, 0, 1) == 5 |
      substring(CultPrincipJardins__2, 0, 1) == 5 |
      substring(CultPrincipJardins__3, 0, 1) == 5 |
      substring(CultPrincipJardins__4, 0, 1) == 5)) |
    (CultPresentesJardins__60 == 0 & (substring(CultPrincipJardins__0, 0, 1) == 6 |
      substring(CultPrincipJardins__1, 0, 1) == 6 |
      substring(CultPrincipJardins__2, 0, 1) == 6 |
      substring(CultPrincipJardins__3, 0, 1) == 6 |
      substring(CultPrincipJardins__4, 0, 1) == 6)) |
    (CultPresentesJardins__70 == 0 & (substring(CultPrincipJardins__0, 0, 1) == 7 |
      substring(CultPrincipJardins__1, 0, 1) == 7 |
      substring(CultPrincipJardins__2, 0, 1) == 7 |
      substring(CultPrincipJardins__3, 0, 1) == 7 |
      substring(CultPrincipJardins__4, 0, 1) == 7)) |
    (CultPresentesJardins__80 == 0 & (substring(CultPrincipJardins__0, 0, 1) == 8 |
      substring(CultPrincipJardins__1, 0, 1) == 8 |
      substring(CultPrincipJardins__2, 0, 1) == 8 |
      substring(CultPrincipJardins__3, 0, 1) == 8 |
      substring(CultPrincipJardins__4, 0, 1) == 8))) |>
  count()
# 74 cas


###########

contenuJardinsOceaniens |>
  mutate(
    CultPresentesJardins__10 = case_when(
      CultPresentesJardins__10 == 0 & (substring(CultPrincipJardins__0, 0, 1) == 1 |
        substring(CultPrincipJardins__1, 0, 1) == 1 |
        substring(CultPrincipJardins__2, 0, 1) == 1 |
        substring(CultPrincipJardins__3, 0, 1) == 1 |
        substring(CultPrincipJardins__4, 0, 1) == 1) ~ 1,
      TRUE ~ CultPresentesJardins__10
    ),
    CultPresentesJardins__20 = case_when(
      CultPresentesJardins__20 == 0 & (substring(CultPrincipJardins__0, 0, 1) == 2 |
        substring(CultPrincipJardins__1, 0, 1) == 2 |
        substring(CultPrincipJardins__2, 0, 1) == 2 |
        substring(CultPrincipJardins__3, 0, 1) == 2 |
        substring(CultPrincipJardins__4, 0, 1) == 2) ~ 1,
      TRUE ~ CultPresentesJardins__20
    ),
    CultPresentesJardins__30 = case_when(
      CultPresentesJardins__30 == 0 & (substring(CultPrincipJardins__0, 0, 1) == 3 |
        substring(CultPrincipJardins__1, 0, 1) == 3 |
        substring(CultPrincipJardins__2, 0, 1) == 3 |
        substring(CultPrincipJardins__3, 0, 1) == 3 |
        substring(CultPrincipJardins__4, 0, 1) == 3) ~ 1,
      TRUE ~ CultPresentesJardins__30
    ),
      CultPresentesJardins__40 = case_when(
        CultPresentesJardins__40 == 0 & (substring(CultPrincipJardins__0, 0, 1) == 4 |
          substring(CultPrincipJardins__1, 0, 1) == 4 |
          substring(CultPrincipJardins__2, 0, 1) == 4 |
          substring(CultPrincipJardins__3, 0, 1) == 4 |
          substring(CultPrincipJardins__4, 0, 1) == 4) ~ 1,
        TRUE ~ CultPresentesJardins__40
      ),
      CultPresentesJardins__50 = case_when(
        CultPresentesJardins__50 == 0 & (substring(CultPrincipJardins__0, 0, 1) == 5 |
          substring(CultPrincipJardins__1, 0, 1) == 5 |
          substring(CultPrincipJardins__2, 0, 1) == 5 |
          substring(CultPrincipJardins__3, 0, 1) == 5 |
          substring(CultPrincipJardins__4, 0, 1) == 5) ~ 1,
        TRUE ~ CultPresentesJardins__50
      ),
      CultPresentesJardins__60 = case_when(
        CultPresentesJardins__60 == 0 & (substring(CultPrincipJardins__0, 0, 1) == 6 |
          substring(CultPrincipJardins__1, 0, 1) == 6 |
          substring(CultPrincipJardins__2, 0, 1) == 6 |
          substring(CultPrincipJardins__3, 0, 1) == 6 |
          substring(CultPrincipJardins__4, 0, 1) == 6) ~ 1,
        TRUE ~ CultPresentesJardins__60
      ),
      CultPresentesJardins__70 = case_when(
        CultPresentesJardins__70 == 0 & (substring(CultPrincipJardins__0, 0, 1) == 7 |
          substring(CultPrincipJardins__1, 0, 1) == 7 |
          substring(CultPrincipJardins__2, 0, 1) == 7 |
          substring(CultPrincipJardins__3, 0, 1) == 7 |
          substring(CultPrincipJardins__4, 0, 1) == 7) ~ 1,
        TRUE ~ CultPresentesJardins__70
      ),
      CultPresentesJardins__80 = case_when(
        CultPresentesJardins__80 == 0 & (substring(CultPrincipJardins__0, 0, 1) == 8 |
          substring(CultPrincipJardins__1, 0, 1) == 8 |
          substring(CultPrincipJardins__2, 0, 1) == 8 |
          substring(CultPrincipJardins__3, 0, 1) == 8 |
          substring(CultPrincipJardins__4, 0, 1) == 8) ~ 1,
        TRUE ~ CultPresentesJardins__80
      )
    )
