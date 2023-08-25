rga23 <- readTable("rga23.tab", dossier)

commercialisation <- function(typeProduction, table) {
  for (i in 1:13) {
    variableI <- paste("ModesCom", typeProduction, "__", i, sep = "")
    PartCommerI <- paste("PartCom", typeProduction, "__", i, sep = "")

    pourcentagesI <-
      table |> filter(PourcentMode__id == i)

    print(PartCommerI)

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


PourcentModeViande <- readTable("PourcentModeViande.tab", dossier)
PourcentModeFlorale <- readTable("PourcentModeFlorale.tab", dossier)
PourcentModeFourrages <-
  readTable("PourcentModeFourrages.tab", dossier)
PourcentModeFruit <- readTable("PourcentModeFruit.tab", dossier)
PourcentModeMaraic <- readTable("PourcentModeMaraic.tab", dossier)
PourcentModePepinieres <-
  readTable("PourcentModePepinieres.tab", dossier)
PourcentModePlantes <- readTable("PourcentModePlantes.tab", dossier)
PourcentModeVivri <- readTable("PourcentModeVivri.tab", dossier)
