# Irrigation ?

## Nombre de chefs d'exploitations qui déclarent avoir irrigué
Partie3_irrigation <- rga23_prodVegetales |>
  filter(!is.na(Irrigation)) |>
  mutate(Irrigation = case_when(
    Irrigation == 1 ~ "Oui",
    Irrigation == 2 ~ "Non"
  )) |>
  group_by(Irrigation) |>
  calculPourcentage()
# Irrigation       n
#    Non         1178
#    Oui         1600
writeCSV(Partie3_irrigation)

## Surfaces irriguées parmi les surfaces classiques des exploitants qui irriguent
surfacesClassiquesIrrigueesParExploitant <- rga23_surfacesCultures_HC_HP |>
  group_by(interview__key) |>
  summarize(
    SurfacesIrrigueesClassiques = sum(replace_na(SurfaceIrrig, 0)),
    SurfacesTotalesClassiques = sum(replace_na(SurfaceCult, 0))
  ) |>
  filter(SurfacesIrrigueesClassiques > 0)
## 1351 exploitants ont déclaré des surfaces classiques irriguées

## Surfaces irriguées parmi les JO des exploitants qui irriguent
surfacesJOIrrigueesParExploitant <- rga23_prodVegetales |>
  group_by(interview__key) |>
  summarize(
    SurfacesIrrigueesJO = sum(replace_na(SurfaceIrrigJardins, 0)),
    SurfacesTotalesJO = sum(replace_na(SurfaceJardins, 0))
  ) |>
  filter(SurfacesIrrigueesJO > 0)
## 56 exploitants ont déclaré des surfaces de JO irriguées

## Porportion de surfaces irriguées parmi les surfaces possédées par les exploitants qui irriguent
Partie3_propSurfacesIrrigueesAuSeinDesExploitationsQuiIrriguent <- full_join(surfacesClassiquesIrrigueesParExploitant, surfacesJOIrrigueesParExploitant, by = "interview__key") |>
  mutate(
    surfacesIrrigueesTotalesDeclarees = replace_na(SurfacesIrrigueesClassiques, 0) + replace_na(SurfacesIrrigueesJO, 0),
    surfacesTotalesDeclarees = replace_na(SurfacesTotalesClassiques, 0) + replace_na(SurfacesTotalesJO, 0)
  ) |>
  summarize(
    proportion = round(sum(surfacesIrrigueesTotalesDeclarees) / sum(surfacesTotalesDeclarees) * 100)
  )
writeCSV(Partie3_propSurfacesIrrigueesAuSeinDesExploitationsQuiIrriguent)

## Retour à l'ensemble des surfaces déclarées
surfacesIrrigueesCultClassiques <- rga23_surfacesCultures_HC_HP |>
  group_by(TypeCultureTexte) |>
  summarize(
    SurfacesIrriguees = sum(replace_na(SurfaceIrrig, 0)),
    SurfacesTotales = sum(replace_na(SurfaceCult, 0)),
    proportion = round(SurfacesIrriguees / SurfacesTotales * 100, 1)
  )

surfacesIrrigueesJO <- rga23_prodVegetales |>
  mutate(TypeCultureTexte = "Jardins océaniens") |>
  group_by(TypeCultureTexte) |>
  summarize(
    SurfacesIrriguees = sum(replace_na(SurfaceIrrigJardins, 0)),
    SurfacesTotales = sum(replace_na(SurfaceJardins, 0)),
    proportion = round(SurfacesIrriguees / SurfacesTotales * 100, 1)
  )

surfacesIrrigueesParTypeCult <- rbind(surfacesIrrigueesCultClassiques, surfacesIrrigueesJO)

Partie3_proportionIrrigueeSurLEnsemble <- surfacesIrrigueesParTypeCult |>
  summarize(
    sommeSurfacesIrriguees = sum(SurfacesIrriguees),
    sommeSurfacesTotales = sum(SurfacesTotales),
    proportion = round(sum(SurfacesIrriguees) / sum(SurfacesTotales) * 100, 4)
  )
writeCSV(Partie3_proportionIrrigueeSurLEnsemble)

Partie3_propSurfacesIrrigueesParTypeCult <- surfacesIrrigueesParTypeCult |>
  select(TypeCultureTexte, proportion)
writeCSV(Partie3_propSurfacesIrrigueesParTypeCult)

# OrigineEauIrrig
# Réseau collectif agricole.....1
# Réseau collectif (communal)...3
# Réseau individuel.............2

Partie3_reseauCollectifAgricole <- rga23_prodVegetales |>
  filter(!is.na(OrigineEauIrrig__1)) |>
  group_by(OrigineEauIrrig__1) |>
  calculPourcentage()
writeCSV(Partie3_reseauCollectifAgricole)

Partie3_reseauIndividuel <- rga23_prodVegetales |>
  filter(!is.na(OrigineEauIrrig__2)) |>
  group_by(OrigineEauIrrig__2) |>
  calculPourcentage()
writeCSV(Partie3_reseauIndividuel)

Partie3_reseauCollectifCommunal <- rga23_prodVegetales |>
  filter(!is.na(OrigineEauIrrig__3)) |>
  group_by(OrigineEauIrrig__3) |>
  calculPourcentage()
writeCSV(Partie3_reseauCollectifCommunal)

# Aspersion.....................................1
# Goutte à goutte...............................2
# Micro-asperseurs..............................3
# Manuellement (tuyau, cuve sur tracteur).......4
# Autres canaux d'irrigation (tarodière, ...)...5

Partie3_modeIrrigation <- rga23_prodVegetales |>
  filter(Irrigation == 1) |>
  summarize(
    aspersion = round(sum(replace_na(ModeIrrigation__1, 0) / n() * 100)),
    goutteAGoutte = round(sum(replace_na(ModeIrrigation__2, 0) / n() * 100)),
    microAsperseurs = round(sum(replace_na(ModeIrrigation__3, 0) / n() * 100)),
    manuellement = round(sum(replace_na(ModeIrrigation__4, 0) / n() * 100)),
    autresCanaux = round(sum(replace_na(ModeIrrigation__5, 0) / n() * 100))
  )
writeCSV(Partie3_modeIrrigation)

# Surface agricole utilisée par type de faire-valoir (parcelles)

surfacesParFaireValoir <- rga23_parcelles |>
  filter(faireValoirParcelle != -999999999) |>
  mutate(`Faire valoir` = case_when(
    faireValoirParcelle == 1 ~ "1 - Propriétaire du terrain",
    faireValoirParcelle == 2 ~ "2 - Locataire pays",
    faireValoirParcelle == 3 ~ "3 - Autre locataire",
    faireValoirParcelle == 4 ~ "4 - Métayer - gardien",
    faireValoirParcelle == 5 ~ "5 - Usufruitier",
    faireValoirParcelle == 6 ~ "6 - Occupant sans titre (occupation précaire)",
    faireValoirParcelle == 7 ~ "7 - Co-indivisaire (propriétaire en indivision)",
    TRUE ~ as.character(faireValoirParcelle)
  )) |>
  group_by(`Faire valoir`) |>
  summarize(`Surface (en Ha)` = round((sum(polygone__area, na.rm = TRUE) + sum(surfaceParcelleNonDelimitee, na.rm = TRUE)) / 10000, 1))

surfacesParcelles <- sum(surfacesParFaireValoir$`Surface (en Ha)`)

sommeSAU_C <- rga23_prodVegetales |>
  filter(lettre_unite == "C") |>
  summarize(SurfacesDesCoprahPurs = round(sum(SurfaceTotalProdAgri) / 10000))

surfacesParFaireValoirPercent <- surfacesParFaireValoir |>
  mutate(Pourcentage = round(`Surface (en Ha)` / surfacesParcelles * 100, 1)) |>
  select(`Faire valoir`, `Surface (en Ha)`, Pourcentage)

surfaces <- rga23_prodVegetales |> summarize(
  SAUTotale = round(sum(SurfaceTotalProdAgri, na.rm = TRUE) / 10000),
  SurfaceTotaleDeclaree = round(sum(totalSurfDeclarees, na.rm = TRUE) / 10000)
)

## Surface archipel
surfacesCulturesTab <- rga23_surfacesCultures |>
  mutate(TypeCulture = case_when(
    (TypeCulture == 10) ~ "10 - Cultures maraîchères",
    (TypeCulture == 20) ~ "20 - Cultures vivrières",
    (TypeCulture == 30) ~ "30 - Cultures fruitières (hors pépinères) et bois d'oeuvre",
    (TypeCulture == 40) ~ "40 - Feuillages et cultures florales (hors pépinières)",
    (TypeCulture == 50) ~ "50 - Plantes aromatiques, stimulantes et médicinales",
    (TypeCulture == 60) ~ "60 - Pépinières (plantes vendues en pot)",
    (TypeCulture == 70) ~ "70 - Cultures fourragères",
    (TypeCulture == 80) ~ "80 - Jachères",
    TRUE ~ as.character(TypeCulture)
  )) |>
  group_by(TypeCulture) |>
  summarize(
    `Nb Exploitants` = n_distinct(interview__key),
    `Surface (Ha)` = round((sum(SurfaceCult, na.rm = TRUE) / 10000), 1),
    `Surface moyenne (m²)` = round(sum(SurfaceCult, na.rm = TRUE) / `Nb Exploitants`)
  )

surfaceTotaleClassiques <- as.numeric(sum(surfacesCulturesTab$`Surface (Ha)`, na.rm = TRUE))
nbExploitantsTotalClassiques <- as.integer(rga23_prodVegetales |> filter(ModesProduction__1 == 1) |> count())

surfacesTypeCulturesEtTotal <- surfacesCulturesTab |>
  add_row(
    TypeCulture = "Total cultures classiques",
    `Nb Exploitants` = nbExploitantsTotalClassiques,
    `Surface (Ha)` = surfaceTotaleClassiques,
    `Surface moyenne (m²)` = as.numeric(NA)
  )

surfacesJardinsOceaniensTab <- rga23_prodVegetales |>
  filter(ModesProduction__4 == 1) |>
  mutate(SurfaceBioJardins = case_when(
    SurfaceBioJardins == 1 ~ SurfaceJardins,
    TRUE ~ 0
  )) |>
  summarize(
    `Nb Exploitants` = n_distinct(interview__key),
    `Surface (Ha)` = round((sum(SurfaceJardins, na.rm = TRUE) / 10000), 1),
    `Surface moyenne (m²)` = round(mean(SurfaceJardins, na.rm = TRUE), 0)
  )

surfaceTotale <- surfaceTotaleClassiques + surfacesJardinsOceaniensTab$`Surface (Ha)`
nbExploitantsTotal <- as.integer(rga23_prodVegetales |> filter(ModesProduction__1 == 1 | ModesProduction__4 == 1) |> count())

surfacesCulturesClassEtOceaniens <- surfacesTypeCulturesEtTotal |>
  add_row(
    TypeCulture = "Jardins Oceaniens",
    `Nb Exploitants` = surfacesJardinsOceaniensTab$`Nb Exploitants`,
    `Surface (Ha)` = surfacesJardinsOceaniensTab$`Surface (Ha)`,
    `Surface moyenne (m²)` = surfacesJardinsOceaniensTab$`Surface moyenne (m²)`
  ) |>
  add_row(
    TypeCulture = "Total",
    `Nb Exploitants` = nbExploitantsTotal,
    `Surface (Ha)` = surfaceTotale,
    `Surface moyenne (m²)` = as.numeric(NA)
  )

# Auto-consommation familiale.....................................1/1
# Alimentation des animaux .......................................2/2
# Dons (à la famille, des amis)...................................3/3
# Echange.........................................................4/4
# Vente directe au particulier ...................................5/5
# Vente par internet (Facebook ou autre site).....................6/6
# Vente à un commerçant, artisan ou revendeur.....................7/7
# Vente à un grossiste............................................8/8
# Vente à un transformateur ou préparateur (y compris abattoir)...9/9
# Vente à la coopérative ou au syndicat...........................10/10
# Vente à la restauration collective..............................11/11
# Vente aux restaurants (hors collectifs) / hôtels................12/12
# Sans objet (pas de production de ce type).......................13/13

calculPartsDestination <- function(partComVar, destinationVar, libelleDestination, surfaceConcernee) {
  result <- rga23_prodVegetales |>
    filter(!is.na({{ partComVar }}) & {{ surfaceConcernee }} > 0) |>
    mutate(
      {{ destinationVar }} := case_when(
        {{ partComVar }} <= 25 ~ paste("0 à 25%", !!libelleDestination),
        {{ partComVar }} <= 50 ~ paste("25 et 50%", !!libelleDestination),
        {{ partComVar }} <= 75 ~ paste("50 et 75%", !!libelleDestination),
        {{ partComVar }} <= 100 ~ paste("Plus de 75%", !!libelleDestination)
      )
    ) |>
    group_by({{ destinationVar }}) |>
    summarise(`Nb exploitants` = n()) |>
    mutate(`En %` = round(`Nb exploitants` / sum(`Nb exploitants`) * 100, 1))

  return(result)
}

autoConsoMaraicha <- calculPartsDestination(PartComMaraic__1, Maraichage, "AutoConsommation", totalSurfaceMarai)
# writeCSV(autoConsoMaraicha)
autoConsoVivrier <- calculPartsDestination(PartComVivri__1, Vivrier, "AutoConsommation", totalSurfaceVivri)
# writeCSV(autoConsoVivrier)
autoConsoFruit <- calculPartsDestination(PartComFruit__1, Fruitier, "AutoConsommation", totalSurfaceFruit)
# writeCSV(autoConsoFruit)
autoConsoPlantes <- calculPartsDestination(PartComPlantes__1, PPAM, "AutoConsommation", totalSurfacePlantes)
# writeCSV(autoConsoPlantes)
autoConsoFlorales <- calculPartsDestination(PartComFlorale__1, Florales, "AutoConsommation", totalSurfaceFlorale)
# writeCSV(autoConsoFlorales)
autoConsoPepinieres <- calculPartsDestination(PartComPepinieres__1, Pepinieres, "AutoConsommation", totalSurfacePepinieres)
# writeCSV(autoConsoPepinieres)

#############

## Détail des surfaces par culture et archipel

surfacesParCultureEtArchipel <- left_join(rga23_surfacesCultures, rga23_champ |> select(interview__key, Archipel_1), by = "interview__key") |>
  group_by(Archipel_1, culture_id) |>
  summarize(
    `Nb Exploitants` = n_distinct(interview__key),
    `Surface (m2)` = sum(SurfaceCult, na.rm = TRUE)
  )

surfacesParCulture <- rga23_surfacesCultures |>
  mutate(Archipel_1 = "Total") |>
  group_by(Archipel_1, culture_id) |>
  summarize(
    `Nb Exploitants` = n_distinct(interview__key),
    `Surface (m2)` = sum(SurfaceCult, na.rm = TRUE)
  )

surfacesParCultureArchipelEtTotal <- rbind(surfacesParCultureEtArchipel, surfacesParCulture) |>
  pivot_wider(names_from = Archipel_1, values_from = c(`Nb Exploitants`, `Surface (m2)`), values_fill = 0)

writeCSV(surfacesParCultureArchipelEtTotal)

## Tableaux pour la partie CULTURES

## Surfaces de cultures classiques par type et archipel - HORS COCOTERAIES ET HORS CULTURES FOURRAGERES

surfacesParType_HC_HF_Archipel <- left_join(
  rga23_surfacesCultures_HC_HP |> filter(TypeCulture != 70),
  rga23_champ |> select(interview__key, Archipel_1),
  by = "interview__key"
) |>
  group_by(Archipel_1, TypeCultureTexte) |>
  summarize(
    `Nb Exploitants` = n_distinct(interview__key),
    `Surface (m2)` = sum(SurfaceCult, na.rm = TRUE),
    `Surface moyenne (m2)` = round(`Surface (m2)` / `Nb Exploitants`)
  )

surfacesParType_HC_HF <- rga23_surfacesCultures_HC_HP |>
  filter(TypeCulture != 70) |>
  mutate(Archipel_1 = "Total") |>
  group_by(Archipel_1, TypeCultureTexte) |>
  summarize(
    `Nb Exploitants` = n_distinct(interview__key),
    `Surface (m2)` = sum(SurfaceCult, na.rm = TRUE),
    `Surface moyenne (m2)` = round(`Surface (m2)` / `Nb Exploitants`)
  )

surfacesParTypeCultureArchipelEtTotal <- rbind(
  surfacesParType_HC_HF_Archipel,
  surfacesParType_HC_HF
) |>
  pivot_wider(names_from = c(Archipel_1), values_from = c(`Nb Exploitants`, `Surface (m2)`, `Surface moyenne (m2)`), values_fill = 0)

writeCSV(surfacesParTypeCultureArchipelEtTotal)


## Surfaces de cultures classiques par type, sexe de l'exploitant et archipel

surfacesParType_HC_HF_Archipel_sexe <- left_join(
  rga23_surfacesCultures_HC_HP |> filter(TypeCulture != 70),
  rga23_champ |> select(interview__key, Archipel_1),
  by = "interview__key"
) |>
  left_join(rga23_mainOeuvre |> select(interview__key, SexeChefExpl),
    by = "interview__key"
  ) |>
  group_by(Archipel_1, TypeCultureTexte, SexeChefExpl) |>
  summarize(
    `Nb Exploitants` = n_distinct(interview__key),
    `Surface (m2)` = sum(SurfaceCult, na.rm = TRUE),
    `Surface moyenne (m2)` = round(`Surface (m2)` / `Nb Exploitants`)
  )

surfacesParType_HC_HF_sexe <- rga23_surfacesCultures_HC_HP |>
  filter(TypeCulture != 70) |>
  left_join(rga23_mainOeuvre |> select(interview__key, SexeChefExpl),
    by = "interview__key"
  ) |>
  mutate(Archipel_1 = "Total") |>
  group_by(Archipel_1, TypeCultureTexte, SexeChefExpl) |>
  summarize(
    `Nb Exploitants` = n_distinct(interview__key),
    `Surface (m2)` = sum(SurfaceCult, na.rm = TRUE),
    `Surface moyenne (m2)` = round(`Surface (m2)` / `Nb Exploitants`)
  )

Partie3_surfacesParTypeCultureArchipelEtTotalSexe <- rbind(
  surfacesParType_HC_HF_Archipel_sexe,
  surfacesParType_HC_HF_sexe
) |>
  pivot_wider(names_from = c(Archipel_1, SexeChefExpl), values_from = c(`Nb Exploitants`, `Surface (m2)`, `Surface moyenne (m2)`), values_fill = 0)

writeCSV(Partie3_surfacesParTypeCultureArchipelEtTotalSexe)

## Jardins océaniens

Partie3_surfacesJOArchipel <- rga23_prodVegetales |>
  filter(ModesProduction__4 == 1) |>
  mutate(
    SurfaceBioJardins = case_when(
      SurfaceBioJardins == 1 ~ SurfaceJardins,
      TRUE ~ 0
    ),
    TypeCultureTexte = "Jardins océaniens"
  ) |>
  group_by(Archipel_1, TypeCultureTexte) |>
  summarize(
    `Nb Exploitants` = n_distinct(interview__key),
    `Surface (m2)` = sum(SurfaceJardins, na.rm = TRUE),
    `Surface moyenne (m2)` = round(`Surface (m2)` / `Nb Exploitants`),
  )

writeCSV(Partie3_surfacesJOArchipel)

ensembleSurfacesTypeJoArchipel <- rbind(surfacesParType_HC_HF_Archipel, Partie3_surfacesJOArchipel)

surfacesJOArchipelSexe <- rga23_prodVegetales |>
  filter(ModesProduction__4 == 1) |>
  left_join(rga23_mainOeuvre |> select(interview__key, SexeChefExpl),
    by = "interview__key"
  ) |>
  mutate(
    SurfaceBioJardins = case_when(
      SurfaceBioJardins == 1 ~ SurfaceJardins,
      TRUE ~ 0
    ),
    TypeCultureTexte = "Jardins océaniens"
  ) |>
  group_by(Archipel_1, TypeCultureTexte, SexeChefExpl) |>
  summarize(
    `Nb Exploitants` = n_distinct(interview__key),
    `Surface (m2)` = sum(SurfaceJardins, na.rm = TRUE),
    `Surface moyenne (m2)` = round(`Surface (m2)` / `Nb Exploitants`),
  )

surfacesJOSexe <- left_join(
  rga23_prodVegetales |> filter(ModesProduction__4 == 1),
  rga23_mainOeuvre |> select(interview__key, SexeChefExpl),
  by = "interview__key"
) |>
  mutate(
    TypeCultureTexte = "Jardins océaniens",
    Archipel_1 = "Total"
  ) |>
  group_by(Archipel_1, TypeCultureTexte, SexeChefExpl) |>
  summarize(
    `Nb Exploitants` = n_distinct(interview__key),
    `Surface (m2)` = sum(SurfaceJardins, na.rm = TRUE),
    `Surface moyenne (m2)` = round(`Surface (m2)` / `Nb Exploitants`),
  )

Partie3_surfacesJoArchipelEtTotalSexe <- rbind(
  surfacesJOArchipelSexe,
  surfacesJOSexe
) |>
  pivot_wider(names_from = c(Archipel_1, SexeChefExpl), values_from = c(`Nb Exploitants`, `Surface (m2)`, `Surface moyenne (m2)`), values_fill = 0)

writeCSV(Partie3_surfacesJoArchipelEtTotalSexe)

### Tableau pour l'encadré

surfaceCultFourrageres <- rga23_surfacesCultures |>
  filter(TypeCulture == 70) |>
  mutate(TypeCultureTexte = case_when(
    (culture_id == 701) ~ "70a - Cultures fourragères : pâturages",
    (culture_id == 702) ~ "70a - Cultures fourragères : pâturages",
    (culture_id == 703) ~ "70b - Cultures fourragères : maïs fourrage et ensilage et sorgho",
    (culture_id == 704) ~ "70b - Cultures fourragères : maïs fourrage et ensilage et sorgho",
    (culture_id == 705) ~ "70a - Cultures fourragères : pâturages",
    TRUE ~ as.character(TypeCulture)
  )) |>
  group_by(TypeCultureTexte) |>
  summarize(
    `Surface (m2)` = sum(SurfaceCult, na.rm = TRUE)
  )

encadreSAUTypeTotal <- rbind(
  surfaceCultFourrageres,
  surfacesParType_HC_HF |> ungroup() |>
    select(-Archipel_1, -`Nb Exploitants`, -`Surface moyenne (m2)`),
  surfacesJO <- rga23_prodVegetales |>
    filter(ModesProduction__4 == 1) |>
    mutate(
      TypeCultureTexte = "Jardins océaniens"
    ) |>
    group_by(TypeCultureTexte) |>
    summarize(`Surface (m2)` = sum(SurfaceJardins, na.rm = TRUE))
) |>
  arrange(TypeCultureTexte)
Partie3_encadreSAUTypeTotalHa <- encadreSAUTypeTotal |>
  mutate(`Surface (Ha)` = round(`Surface (m2)` / 10000)) |>
  select(-`Surface (m2)`)
writeCSV(Partie3_encadreSAUTypeTotalHa)

hectaresCulturesVegetales <- encadreSAUTypeTotal |>
  filter(TypeCultureTexte != "70a - Cultures fourragères : pâturages") |>
  summarize(`Surface (Ha)` = round(sum(`Surface (m2)`) / 10000))

Partie3_EncadreSurfacePaturagesArchipel <- left_join(
  rga23_surfacesCultures,
  rga23_champ |> select(interview__key, Archipel_1),
  by = "interview__key"
) |>
  filter(culture_id == 701 | culture_id == 702 | culture_id == 705) |>
  group_by(Archipel_1) |>
  summarize(
    `Surface de pâturages (Ha)` = round(sum(SurfaceCult, na.rm = TRUE) / 10000)
  )
writeCSV(Partie3_EncadreSurfacePaturagesArchipel)

### Surfaces par archipel HORS paturages et hors cocoteraies

surfacesParArchipel <- rbind(
  Partie3_surfacesJOArchipel |>
    select(-`Surface moyenne (m2)`, `Nb Exploitants`),
  left_join(
    rga23_surfacesCultures_HC_HP,
    rga23_champ |> select(interview__key, Archipel_1),
    by = "interview__key"
  ) |>
    group_by(Archipel_1) |>
    summarize(
      `Surface (m2)` = sum(SurfaceCult, na.rm = TRUE)
    )
) |>
  group_by(Archipel_1) |>
  summarize(
    `Surface de productions végétales (Ha)` = round(sum(`Surface (m2)`) / 10000)
  )

jacheres <- left_join(
  rga23_surfacesCultures |>
    filter(TypeCulture == 80),
  rga23_champ |> select(interview__key, Archipel_1),
  by = "interview__key"
) |>
  group_by(Archipel_1) |>
  summarize(
    `dont jachères (Ha)` = round(sum(SurfaceCult, na.rm = TRUE) / 10000)
  )

avecJacheres <- left_join(surfacesParArchipel,
  jacheres,
  by = "Archipel_1"
)
