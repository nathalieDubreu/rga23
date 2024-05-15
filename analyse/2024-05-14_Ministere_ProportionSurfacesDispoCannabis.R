# Proportion surfaces disponibles par archipel

## Champ identique au RGA

## Relance des programmes jusqu'à la table de surface dont jachères (avecJachères)
source("analyse/Partie0_PreparationDesTables.R")
source("analyse/Partie3_ProdVegetales.R")

avecJacheres

SAU_par_archipel <- rga23_prodVegetales |>
  group_by(Archipel_1) |>
  summarize(
    SAUTotale = round(sum(SurfaceTotalProdAgri, na.rm = TRUE) / 10000)
  )

resultatsArchipel <- left_join(SAU_par_archipel, avecJacheres, by = "Archipel_1") |>
  rename(
    Jacheres = `dont jachÃ¨res (Ha)`,
    SurfaceProductionsVegetales = `Surface de productions vÃ©gÃ©tales (Ha)`
  ) |>
  mutate(ProportionSurfaceDisponibleJacheres = 0.3 / Jacheres,
         ProportionSurfaceDisponibleProductionsVegetales = 0.3 / SurfaceProductionsVegetales )

total <- resultatsArchipel |>
  summarize(
  Archipel_1 = "Total",
  SAUTotale = sum(SAUTotale),
  SurfaceProductionsVegetales = sum(SurfaceProductionsVegetales),
  Jacheres = sum(Jacheres),
  ProportionSurfaceDisponibleJacheres = "/",
  ProportionSurfaceDisponibleProductionsVegetales = "/"
)

Demande_2024_05_14 <- rbind(resultatsArchipel, total)
writeCSV(Demande_2024_05_14)

