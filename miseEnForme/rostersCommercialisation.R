rga23 <- readTable("rga23.tab", dossier)

commercialisation <- function(typeProduction, table) {
  for (i in 1:13) {
    variableI <- paste("ModesCom", typeProduction, "__", i, sep = "")
    PartCommerI <- paste("PartCom", typeProduction, "__", i, sep = "")

    pourcentagesI <-
      table |> filter(PourcentMode__id == i)

    rga23 <-
      left_join(
        rga23,
        pourcentagesI |> select(!PourcentMode__id),
        by = c("interview__id", "interview__key")
      ) |>
      rename("{PartCommerI}" := PourcentCom) |>
      select(!variableI)
  }
  return(rga23)
}

TablePourcentMode <- readTable("PourcentModeMiel.tab", dossier) %>% rename(
  PourcentMode__id = PourcentModeMiel__id,
  PourcentCom = PourcentComMiel
)
rga23 <- commercialisation("Miel", TablePourcentMode)


TablePourcentMode <- readTable("PourcentModeOeufs.tab", dossier) %>% rename(
  PourcentMode__id = PourcentModeOeufs__id,
  PourcentCom = PourcentComOeufs
)
rga23 <- commercialisation("Oeufs", TablePourcentMode)


TablePourcentMode <- readTable("PourcentModeViande.tab", dossier) %>% rename(
  PourcentMode__id = PourcentModeViande__id,
  PourcentCom = PourcentComViande
)
rga23 <- commercialisation("Viande", TablePourcentMode)


TablePourcentMode <- readTable("PourcentModeFlorale.tab", dossier) %>% rename(
  PourcentMode__id = PourcentModeFlorale__id,
  PourcentCom = PourcentComFlorale
)
rga23 <- commercialisation("Florale", TablePourcentMode)


TablePourcentMode <- readTable("PourcentModeFourrages.tab", dossier) %>% rename(
  PourcentMode__id = PourcentModeFourrages__id,
  PourcentCom = PourcentComFourrages
)
rga23 <- commercialisation("Fourrages", TablePourcentMode)


TablePourcentMode <- readTable("PourcentModeFruit.tab", dossier) %>% rename(
  PourcentMode__id = PourcentModeFruit__id,
  PourcentCom = PourcentComFruit
)
rga23 <- commercialisation("Fruit", TablePourcentMode)


TablePourcentMode <- readTable("PourcentModeMaraic.tab", dossier) %>% rename(
  PourcentMode__id = PourcentModeMaraic__id,
  PourcentCom = PourcentComMaraich
)
rga23 <- commercialisation("Maraic", TablePourcentMode)


TablePourcentMode <- readTable("PourcentModePepinieres.tab", dossier) %>% rename(
  PourcentMode__id = PourcentModePepinieres__id,
  PourcentCom = PourcentComPepinieres
)
rga23 <- commercialisation("Pepinieres", TablePourcentMode)


TablePourcentMode <- readTable("PourcentModePlantes.tab", dossier) %>% rename(
  PourcentMode__id = PourcentModePlantes__id,
  PourcentCom = PourcentComPlantes
)
rga23 <- commercialisation("Plantes", TablePourcentMode)


TablePourcentMode <- readTable("PourcentModeVivri.tab", dossier) %>% rename(
  PourcentMode__id = PourcentModeVivri__id,
  PourcentCom = PourcentComVivri
)
rga23 <- commercialisation("Vivri", TablePourcentMode)
