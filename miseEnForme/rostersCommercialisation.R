commercialisation <- function(typeProduction, roster, rga23) {
  for (i in 1:13) {
    variableI <- paste("ModesCom", typeProduction, "__", i, sep = "")
    PartCommerI <- paste("PartCom", typeProduction, "__", i, sep = "")

    pourcentagesI <-
      roster |> filter(PourcentMode__id == i)

    rga23 <-
      left_join(
        rga23,
        pourcentagesI |> select(!PourcentMode__id & !interview__id),
        by = c("interview__key")
      ) |>
      rename("{PartCommerI}" := PourcentCom) |>
      select(!variableI)
  }
  return(rga23)
}

## MAJ rga23_prodAnimales

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


## MAJ rga23_prodVegetales

TablePourcentMode <- readTable("PourcentModeFlorale.tab", dossier) %>% rename(
  PourcentMode__id = PourcentModeFlorale__id,
  PourcentCom = PourcentComFlorale
)
rga23_prodVegetales <- commercialisation("Florale", TablePourcentMode, rga23_prodVegetales)


TablePourcentMode <- readTable("PourcentModeFourrages.tab", dossier) %>% rename(
  PourcentMode__id = PourcentModeFourrages__id,
  PourcentCom = PourcentComFourrages
)
rga23_prodVegetales <- commercialisation("Fourrages", TablePourcentMode, rga23_prodVegetales)


TablePourcentMode <- readTable("PourcentModeFruit.tab", dossier) %>% rename(
  PourcentMode__id = PourcentModeFruit__id,
  PourcentCom = PourcentComFruit
)
rga23_prodVegetales <- commercialisation("Fruit", TablePourcentMode, rga23_prodVegetales)


TablePourcentMode <- readTable("PourcentModeMaraic.tab", dossier) %>% rename(
  PourcentMode__id = PourcentModeMaraic__id,
  PourcentCom = PourcentComMaraich
)
rga23_prodVegetales <- commercialisation("Maraic", TablePourcentMode, rga23_prodVegetales)


TablePourcentMode <- readTable("PourcentModePepinieres.tab", dossier) %>% rename(
  PourcentMode__id = PourcentModePepinieres__id,
  PourcentCom = PourcentComPepinieres
)
rga23_prodVegetales <- commercialisation("Pepinieres", TablePourcentMode, rga23_prodVegetales)


TablePourcentMode <- readTable("PourcentModePlantes.tab", dossier) %>% rename(
  PourcentMode__id = PourcentModePlantes__id,
  PourcentCom = PourcentComPlantes
)
rga23_prodVegetales <- commercialisation("Plantes", TablePourcentMode, rga23_prodVegetales)


TablePourcentMode <- readTable("PourcentModeVivri.tab", dossier) %>% rename(
  PourcentMode__id = PourcentModeVivri__id,
  PourcentCom = PourcentComVivri
)
rga23_prodVegetales <- commercialisation("Vivri", TablePourcentMode, rga23_prodVegetales)


# Export fichiers traités
writeCSVTraites(rga23_prodAnimales)
writeCSVTraites(rga23_prodVegetales)

