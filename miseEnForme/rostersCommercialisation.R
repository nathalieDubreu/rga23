commercialisation <- function(typeProduction, roster, rga23) {
  for (i in 1:13) {
    variableI <- paste("ModesCom", typeProduction, "__", i, sep = "")
    PartCommerI <- paste("PartCom", typeProduction, "__", i, sep = "")

    pourcentagesI <-
      roster |> filter(PourcentMode__id == i)

    rga23 <-
      ## ETAPE 1 : Récupération des pourcentages présents dans les rosters
      left_join(
        rga23,
        pourcentagesI |> select(!PourcentMode__id & !interview__id),
        by = c("interview__key")
      ) |>
      ## ETAPE 2 : Imputation de valeurs -> 100% s'il n'y a qu'une modalité
      mutate(PourcentCom = case_when(
        (eval(parse(text = variableI)) == 1 & is.na(PourcentCom)) ~ 100,
        TRUE ~ as.numeric(PourcentCom)
      )) |>
      rename("{PartCommerI}" := PourcentCom)
    # |>
    #   select(!variableI)
  }
  return(rga23)
}


### MAJ rga23_prodAnimales

TablePourcentMode <- readTable("PourcentModeMiel.tab", dossier) %>% rename(
  PourcentMode__id = PourcentModeMiel__id,
  PourcentCom = PourcentComMiel
)
rga23_prodAnimales <- commercialisation("Miel", TablePourcentMode, rga23_prodAnimales)


TablePourcentMode <- readTable("PourcentModeOeufs.tab", dossier) %>% rename(
  PourcentMode__id = PourcentModeOeufs__id,
  PourcentCom = PourcentComOeufs
)
rga23_prodAnimales <- commercialisation("Oeufs", TablePourcentMode, rga23_prodAnimales)

TablePourcentMode <- readTable("PourcentModeViande.tab", dossier) %>% rename(
  PourcentMode__id = PourcentModeViande__id,
  PourcentCom = PourcentComViande
)
rga23_prodAnimales <- commercialisation("Viande", TablePourcentMode, rga23_prodAnimales)


### MAJ rga23_prodVegetales

TablePourcentMode <- readTable("PourcentModeMaraic.tab", dossier) %>% rename(
  PourcentMode__id = PourcentModeMaraic__id,
  PourcentCom = PourcentComMaraich
)
rga23_prodVegetales <- commercialisation("Maraic", TablePourcentMode, rga23_prodVegetales)

TablePourcentMode <- readTable("PourcentModeVivri.tab", dossier) %>% rename(
  PourcentMode__id = PourcentModeVivri__id,
  PourcentCom = PourcentComVivri
)
rga23_prodVegetales <- commercialisation("Vivri", TablePourcentMode, rga23_prodVegetales)

TablePourcentMode <- readTable("PourcentModeFruit.tab", dossier) %>% rename(
  PourcentMode__id = PourcentModeFruit__id,
  PourcentCom = PourcentComFruit
)
rga23_prodVegetales <- commercialisation("Fruit", TablePourcentMode, rga23_prodVegetales)

TablePourcentMode <- readTable("PourcentModeFlorale.tab", dossier) %>% rename(
  PourcentMode__id = PourcentModeFlorale__id,
  PourcentCom = PourcentComFlorale
)
rga23_prodVegetales <- commercialisation("Florale", TablePourcentMode, rga23_prodVegetales)

TablePourcentMode <- readTable("PourcentModePlantes.tab", dossier) %>% rename(
  PourcentMode__id = PourcentModePlantes__id,
  PourcentCom = PourcentComPlantes
)
rga23_prodVegetales <- commercialisation("Plantes", TablePourcentMode, rga23_prodVegetales)

TablePourcentMode <- readTable("PourcentModePepinieres.tab", dossier) %>% rename(
  PourcentMode__id = PourcentModePepinieres__id,
  PourcentCom = PourcentComPepinieres
)
rga23_prodVegetales <- commercialisation("Pepinieres", TablePourcentMode, rga23_prodVegetales)

TablePourcentMode <- readTable("PourcentModeFourrages.tab", dossier) %>% rename(
  PourcentMode__id = PourcentModeFourrages__id,
  PourcentCom = PourcentComFourrages
)
rga23_prodVegetales <- commercialisation("Fourrages", TablePourcentMode, rga23_prodVegetales)

## Export fichiers traités
writeCSVTraites(rga23_prodAnimales)
writeCSVTraites(rga23_prodVegetales)

# test2 <- rga23_prodVegetales |> filter(is.na(PartComFlorale__1) & ModesComFlorale__1 == 1)


# Vérifications

# ## Cultures maraichères - 10
# left_join(rga23_prodVegetales, rga23 |> select(interview__key, ProductionAgricole)) |>
#   mutate(
#     pourcent= ifelse(is.na(PartComMaraic__1), 0, PartComMaraic__1) +
#       ifelse(is.na(PartComMaraic__2), 0, PartComMaraic__2) +
#       ifelse(is.na(PartComMaraic__3), 0, PartComMaraic__3) +
#       ifelse(is.na(PartComMaraic__4), 0, PartComMaraic__4) +
#       ifelse(is.na(PartComMaraic__5), 0, PartComMaraic__5) +
#       ifelse(is.na(PartComMaraic__6), 0, PartComMaraic__6) +
#       ifelse(is.na(PartComMaraic__7), 0, PartComMaraic__7) +
#       ifelse(is.na(PartComMaraic__8), 0, PartComMaraic__8) +
#       ifelse(is.na(PartComMaraic__9), 0, PartComMaraic__9) +
#       ifelse(is.na(PartComMaraic__10), 0, PartComMaraic__10) +
#       ifelse(is.na(PartComMaraic__11), 0, PartComMaraic__11) +
#       ifelse(is.na(PartComMaraic__12), 0, PartComMaraic__12) +
#       ifelse(is.na(PartComMaraic__13), 0, PartComMaraic__13),
#     presenceCultures = ifelse(is.na(CulturesPresentes__10), 0, CulturesPresentes__10) +
#       ifelse(is.na(CultPresentesJardins__10), 0, CultPresentesJardins__10)
#   ) |>
#   group_by(ProductionAgricole, presenceCultures, pourcent) |>
#   count()
# 
# ## Cultures vivrières - 20
# left_join(rga23_prodVegetales, rga23 |> select(interview__key, ProductionAgricole)) |>
#   mutate(
#     pourcent= ifelse(is.na(PartComVivri__1), 0, PartComVivri__1) +
#       ifelse(is.na(PartComVivri__2), 0, PartComVivri__2) +
#       ifelse(is.na(PartComVivri__3), 0, PartComVivri__3) +
#       ifelse(is.na(PartComVivri__4), 0, PartComVivri__4) +
#       ifelse(is.na(PartComVivri__5), 0, PartComVivri__5) +
#       ifelse(is.na(PartComVivri__6), 0, PartComVivri__6) +
#       ifelse(is.na(PartComVivri__7), 0, PartComVivri__7) +
#       ifelse(is.na(PartComVivri__8), 0, PartComVivri__8) +
#       ifelse(is.na(PartComVivri__9), 0, PartComVivri__9) +
#       ifelse(is.na(PartComVivri__10), 0, PartComVivri__10) +
#       ifelse(is.na(PartComVivri__11), 0, PartComVivri__11) +
#       ifelse(is.na(PartComVivri__12), 0, PartComVivri__12) +
#       ifelse(is.na(PartComVivri__13), 0, PartComVivri__13),
#     presenceCultures = ifelse(is.na(CulturesPresentes__20), 0, CulturesPresentes__20) +
#       ifelse(is.na(CultPresentesJardins__20), 0, CultPresentesJardins__20)
#   ) |> 
#   group_by(ProductionAgricole, presenceCultures, pourcent) |>
#   count()
# 
# ## Cultures fruitières - 30
# left_join(rga23_prodVegetales, rga23 |> select(interview__key, ProductionAgricole)) |>
#   mutate(
#     pourcent= ifelse(is.na(PartComFruit__1), 0, PartComFruit__1) +
#       ifelse(is.na(PartComFruit__2), 0, PartComFruit__2) +
#       ifelse(is.na(PartComFruit__3), 0, PartComFruit__3) +
#       ifelse(is.na(PartComFruit__4), 0, PartComFruit__4) +
#       ifelse(is.na(PartComFruit__5), 0, PartComFruit__5) +
#       ifelse(is.na(PartComFruit__6), 0, PartComFruit__6) +
#       ifelse(is.na(PartComFruit__7), 0, PartComFruit__7) +
#       ifelse(is.na(PartComFruit__8), 0, PartComFruit__8) +
#       ifelse(is.na(PartComFruit__9), 0, PartComFruit__9) +
#       ifelse(is.na(PartComFruit__10), 0, PartComFruit__10) +
#       ifelse(is.na(PartComFruit__11), 0, PartComFruit__11) +
#       ifelse(is.na(PartComFruit__12), 0, PartComFruit__12) +
#       ifelse(is.na(PartComFruit__13), 0, PartComFruit__13),
#     presenceCultures = ifelse(is.na(CulturesPresentes__30), 0, CulturesPresentes__30) +
#       ifelse(is.na(CultPresentesJardins__30), 0, CultPresentesJardins__30)
#   ) |> 
#   group_by(ProductionAgricole, presenceCultures, pourcent) |>
#   count()
# 
# ## Cultures florales - 40
# left_join(rga23_prodVegetales, rga23 |> select(interview__key, ProductionAgricole)) |>
#   mutate(
#     pourcent = ifelse(is.na(PartComFlorale__1), 0, PartComFlorale__1) +
#       ifelse(is.na(PartComFlorale__2), 0, PartComFlorale__2) +
#       ifelse(is.na(PartComFlorale__3), 0, PartComFlorale__3) +
#       ifelse(is.na(PartComFlorale__4), 0, PartComFlorale__4) +
#       ifelse(is.na(PartComFlorale__5), 0, PartComFlorale__5) +
#       ifelse(is.na(PartComFlorale__6), 0, PartComFlorale__6) +
#       ifelse(is.na(PartComFlorale__7), 0, PartComFlorale__7) +
#       ifelse(is.na(PartComFlorale__8), 0, PartComFlorale__8) +
#       ifelse(is.na(PartComFlorale__9), 0, PartComFlorale__9) +
#       ifelse(is.na(PartComFlorale__10), 0, PartComFlorale__10) +
#       ifelse(is.na(PartComFlorale__11), 0, PartComFlorale__11) +
#       ifelse(is.na(PartComFlorale__12), 0, PartComFlorale__12) +
#       ifelse(is.na(PartComFlorale__13), 0, PartComFlorale__13),
#     presenceCultures = ifelse(is.na(CulturesPresentes__40), 0, CulturesPresentes__40) +
#       ifelse(is.na(CultPresentesJardins__40), 0, CultPresentesJardins__40)
#   ) |>
#   group_by(ProductionAgricole, presenceCultures, pourcent) |>
#   count()
# 
# ## Cultures plantes aromatiques - 50
# left_join(rga23_prodVegetales, rga23 |> select(interview__key, ProductionAgricole)) |>
#   mutate(
#     pourcent = ifelse(is.na(PartComPlantes__1), 0, PartComPlantes__1) +
#       ifelse(is.na(PartComPlantes__2), 0, PartComPlantes__2) +
#       ifelse(is.na(PartComPlantes__3), 0, PartComPlantes__3) +
#       ifelse(is.na(PartComPlantes__4), 0, PartComPlantes__4) +
#       ifelse(is.na(PartComPlantes__5), 0, PartComPlantes__5) +
#       ifelse(is.na(PartComPlantes__6), 0, PartComPlantes__6) +
#       ifelse(is.na(PartComPlantes__7), 0, PartComPlantes__7) +
#       ifelse(is.na(PartComPlantes__8), 0, PartComPlantes__8) +
#       ifelse(is.na(PartComPlantes__9), 0, PartComPlantes__9) +
#       ifelse(is.na(PartComPlantes__10), 0, PartComPlantes__10) +
#       ifelse(is.na(PartComPlantes__11), 0, PartComPlantes__11) +
#       ifelse(is.na(PartComPlantes__12), 0, PartComPlantes__12) +
#       ifelse(is.na(PartComPlantes__13), 0, PartComPlantes__13),
#     presenceCultures = ifelse(is.na(CulturesPresentes__50), 0, CulturesPresentes__50) +
#       ifelse(is.na(CultPresentesJardins__50), 0, CultPresentesJardins__50)
#   ) |>
#   group_by(ProductionAgricole, presenceCultures, pourcent) |>
#   count()
# 
# ## Cultures pépinières - 60
# left_join(rga23_prodVegetales, rga23 |> select(interview__key, ProductionAgricole)) |>
#   mutate(
#     pourcent = ifelse(is.na(PartComPepinieres__1), 0, PartComPepinieres__1) +
#       ifelse(is.na(PartComPepinieres__2), 0, PartComPepinieres__2) +
#       ifelse(is.na(PartComPepinieres__3), 0, PartComPepinieres__3) +
#       ifelse(is.na(PartComPepinieres__4), 0, PartComPepinieres__4) +
#       ifelse(is.na(PartComPepinieres__5), 0, PartComPepinieres__5) +
#       ifelse(is.na(PartComPepinieres__6), 0, PartComPepinieres__6) +
#       ifelse(is.na(PartComPepinieres__7), 0, PartComPepinieres__7) +
#       ifelse(is.na(PartComPepinieres__8), 0, PartComPepinieres__8) +
#       ifelse(is.na(PartComPepinieres__9), 0, PartComPepinieres__9) +
#       ifelse(is.na(PartComPepinieres__10), 0, PartComPepinieres__10) +
#       ifelse(is.na(PartComPepinieres__11), 0, PartComPepinieres__11) +
#       ifelse(is.na(PartComPepinieres__12), 0, PartComPepinieres__12) +
#       ifelse(is.na(PartComPepinieres__13), 0, PartComPepinieres__13),
#     presenceCultures = ifelse(is.na(CulturesPresentes__60), 0, CulturesPresentes__60)
#   ) |>
#   group_by(ProductionAgricole, presenceCultures, pourcent) |>
#   count()
# 
# ## Cultures fourrages - 70
# left_join(rga23_prodVegetales, rga23 |> select(interview__key, ProductionAgricole)) |>
#   mutate(
#     pourcent = ifelse(is.na(PartComFourrages__1), 0, PartComFourrages__1) +
#       ifelse(is.na(PartComFourrages__2), 0, PartComFourrages__2) +
#       ifelse(is.na(PartComFourrages__3), 0, PartComFourrages__3) +
#       ifelse(is.na(PartComFourrages__4), 0, PartComFourrages__4) +
#       ifelse(is.na(PartComFourrages__5), 0, PartComFourrages__5) +
#       ifelse(is.na(PartComFourrages__6), 0, PartComFourrages__6) +
#       ifelse(is.na(PartComFourrages__7), 0, PartComFourrages__7) +
#       ifelse(is.na(PartComFourrages__8), 0, PartComFourrages__8) +
#       ifelse(is.na(PartComFourrages__9), 0, PartComFourrages__9) +
#       ifelse(is.na(PartComFourrages__10), 0, PartComFourrages__10) +
#       ifelse(is.na(PartComFourrages__11), 0, PartComFourrages__11) +
#       ifelse(is.na(PartComFourrages__12), 0, PartComFourrages__12) +
#       ifelse(is.na(PartComFourrages__13), 0, PartComFourrages__13),
#     presenceCultures = ifelse(is.na(CulturesPresentes__70), 0, CulturesPresentes__70) +
#       ifelse(is.na(CultPresentesJardins__70), 0, CultPresentesJardins__70)
#   ) |>
#   group_by(ProductionAgricole, presenceCultures, pourcent) |>
#   count()
