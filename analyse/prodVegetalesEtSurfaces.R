# Irrigation ?

irrigation <- rga23_prodVegetales |>
  filter(!is.na(Irrigation)) |>
  mutate(Irrigation = case_when(
    Irrigation == 1 ~ "Oui",
    Irrigation == 2 ~ "Non"
  )) |>
  group_by(Irrigation) |>
  calculPourcentage()

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
  summarize(`Surface (en Ha)` = round((sum(polygone__area, na.rm = TRUE) + sum(surfaceParcelleNonDelimitee, na.rm = TRUE)) / 10000), 1)

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
    `Surface moyenne (m²)` = round(mean(SurfaceCult, na.rm = TRUE), 0)
  )

surfaceTotaleClassiques <- as.numeric(sum(surfacesCulturesTab$`Surface (Ha)`, na.rm = TRUE))
nbExploitantsTotalClassiques <- as.integer(rga23_prodVegetales |> filter(ModesProduction__1 == 1) |> count())

surfacesCulturesArchipelEtTotal <- surfacesCulturesTab |>
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

surfacesCulturesClassEtOceaniens <- surfacesCulturesArchipelEtTotal |>
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

calculPartsDestination <- function(partComVar, destinationVar, libelleDestination) {
  result <- rga23_prodVegetales |>
    filter(!is.na({{ partComVar }})) |>
    mutate(
      {{ destinationVar }} := case_when(
        {{ partComVar }} == 0 ~ paste("0%", !!libelleDestination),
        {{ partComVar }} <= 25 ~ paste("1 à 25%", !!libelleDestination),
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

autoConsoMaraicha <- calculPartsDestination(PartComMaraic__1, Maraichage, "AutoConsommation")
autoConsoVivrier <- calculPartsDestination(PartComVivri__1, Vivrier, "AutoConsommation")
autoConsoFruit <- calculPartsDestination(PartComFruit__1, Fruitier, "AutoConsommation")
autoConsoPlantes <- calculPartsDestination(PartComPlantes__1, PPAM, "AutoConsommation")
autoConsoFlorales <- calculPartsDestination(PartComFlorale__1, Florales, "AutoConsommation")
autoConsoPepinieres <- calculPartsDestination(PartComPepinieres__1, Pepinieres, "AutoConsommation")
