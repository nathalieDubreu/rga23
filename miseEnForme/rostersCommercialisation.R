rga23 <- readTable("rga23.tab", dossier)

PourcentModeMiel <- readTable("PourcentModeMiel.tab", dossier)

typeProduction <- "Miel"

for (i in 1:13) {
  variableI <- paste("ModesCom",typeProduction,"__", i, sep = "")
  PartCommerI <- paste("PartCom",typeProduction,"__", i, sep = "")

  pourcentagesI <-
    PourcentModeMiel |> filter(PourcentModeMiel__id == i)

  rga23 <-
    left_join(
      rga23,
      pourcentagesI |> select(!PourcentModeMiel__id),
      by = c("interview__id", "interview__key")
    ) |>
    rename("{PartCommerI}" := PourcentComMiel) |>
    select(!variableI)
}

################# WIP -> passage en fonction #####################

commercialisation <- function(typeProduction, variable) {
  for (i in 1:13) {
    variableI <- paste("ModesCom",typeProduction,"__", i, sep = "")
    PartCommerI <- paste("PartCom",typeProduction,"__", i, sep = "")
    
    pourcentagesI <-
      PourcentModeMiel |> filter(PourcentModeMiel__id == i)
    
    print(PartCommerI)
    
    rga23 <-
      left_join(
        rga23,
        pourcentagesI |> select(!PourcentModeMiel__id),
        by = c("interview__id", "interview__key")
      ) |>
      rename("{PartCommerI}" := PourcentComMiel)  |>
      select(!variableI)
  }
  return(rga23)
}

rga23Ex <- commercialisation("Miel",PourcentModeMiel$PourcentModeMiel__id)


PourcentModeOeufs <- readTable("PourcentModeOeufs.tab", dossier)
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
