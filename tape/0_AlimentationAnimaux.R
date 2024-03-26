# Aliments consommés

## ComplAlimentation -> distinction entre autonomes ou pas du tout

alimentsAchetesExclusivement <- (
  (rga23_prodAnimales$ComplAlimentation__2 == 1 |
    rga23_prodAnimales$ComplAlimentation__3 == 1 |
    rga23_prodAnimales$ComplAlimentation__4 == 1 |
    rga23_prodAnimales$ComplAlimentation__5 == 1) &
    rga23_prodAnimales$ComplAlimentation__1 == 0 &
    rga23_prodAnimales$ComplAlimentation__6 == 0 &
    rga23_prodAnimales$ComplAlimentation__7 == 0 &
    rga23_prodAnimales$ComplAlimentation__8 == 0 &
    rga23_prodAnimales$ComplAlimentation__9 == 0 &
    rga23_prodAnimales$ComplAlimentation__10 == 0)

alimentsPresentsExclusivement <- (
  (rga23_prodAnimales$ComplAlimentation__1 == 1 |
    rga23_prodAnimales$ComplAlimentation__7 == 1) &
    (rga23_prodAnimales$ComplAlimentation__2 == 0 &
      rga23_prodAnimales$ComplAlimentation__3 == 0 &
      rga23_prodAnimales$ComplAlimentation__4 == 0 &
      rga23_prodAnimales$ComplAlimentation__5 == 0 &
      rga23_prodAnimales$ComplAlimentation__6 == 0 &
      rga23_prodAnimales$ComplAlimentation__8 == 0 &
      rga23_prodAnimales$ComplAlimentation__9 == 0 &
      rga23_prodAnimales$ComplAlimentation__10 == 0
    )
)

# AutAlimAnimauxBasseCour
# AutAlimBovinsFourrage
# AutAlimCaprinsFourrage
# AutAlimEquidesFourrages
# AutAlimOvinsFourrage
# AutAlimPorcins
# AutAlimPoules

# Plus de 90%.......................................1
# 75% à moins de 90%................................2
# 50% à moins de 75%................................3
# 25% à moins de 50%................................4
# Moins de 25%......................................5
# Aucune autonomie (tout est acheté)................6
# Sans objet, ce type d'aliment n'est pas utilisé...7

autonomieAlimentaire <- function(niveau1, niveau2) {
  variables <- c(
    "AutAlimAnimauxBasseCour", "AutAlimBovinsFourrage", "AutAlimCaprinsFourrage",
    "AutAlimEquidesFourrages", "AutAlimOvinsFourrage", "AutAlimPorcins", "AutAlimPoules"
  )
  conditions <- lapply(variables, function(col) {
    paste0(
      "(is.na(", paste("rga23_prodAnimales$", col, sep = ""), ") | ",
      paste("rga23_prodAnimales$", col, sep = ""), " == ", niveau1, " | ",
      paste("rga23_prodAnimales$", col, sep = ""), " == ", niveau2, ")"
    )
  })
  condition <- paste0("(", paste(conditions, collapse = " & "), ")")
  return(condition)
}

autonomie_1_2_ <- autonomieAlimentaire(1, 2)
autonomie_2_3_ <- autonomieAlimentaire(2, 3)
autonomie_3_4_ <- autonomieAlimentaire(3, 4)
autonomie_4_5_ <- autonomieAlimentaire(4, 5)
autonomie_5_6_ <- autonomieAlimentaire(5, 6)
autonomie_6_ <- autonomieAlimentaire(6, 6)
autonomie_5_ <- autonomieAlimentaire(5, 5)
autonomie_4_ <- autonomieAlimentaire(4, 4)
autonomie_3_ <- autonomieAlimentaire(3, 3)
autonomie_2_ <- autonomieAlimentaire(2, 2)
autonomie_1_ <- autonomieAlimentaire(1, 1)

rga23_prodAnimales_alimentation <- rga23_prodAnimales |>
  mutate(
    alimentsAchetesExclusivement = case_when(
      alimentsAchetesExclusivement ~ 1,
      RaisonsRecensement__2 == 1 & eval(parse(text = autonomie_6_)) ~ 1,
      TRUE ~ as.numeric(NA)
    ),
    alimentsPresentsExclusivement = case_when(
      alimentsPresentsExclusivement ~ 1,
      TRUE ~ as.numeric(NA)
    ),
    niveauAutonomie = case_when(
      rowSums(across(
        c(
          "AutAlimAnimauxBasseCour", "AutAlimBovinsFourrage", "AutAlimCaprinsFourrage",
          "AutAlimEquidesFourrages", "AutAlimOvinsFourrage", "AutAlimPorcins", "AutAlimPoules"
        ),
        ~ coalesce(., 0)
      )) == 0 ~ as.numeric(NA),
      # Plus de 90%.......................................1
      RaisonsRecensement__2 == 1 & eval(parse(text = autonomie_1_)) ~ 1,
      # 75% à moins de 90%................................2
      RaisonsRecensement__2 == 1 & eval(parse(text = autonomie_2_)) ~ 2,
      # 50% à moins de 75%................................3
      RaisonsRecensement__2 == 1 & eval(parse(text = autonomie_3_)) ~ 3,
      # 25% à moins de 50%................................4
      RaisonsRecensement__2 == 1 & eval(parse(text = autonomie_4_)) ~ 4,
      # Moins de 25%......................................5
      RaisonsRecensement__2 == 1 & eval(parse(text = autonomie_5_)) ~ 5,
      # Aucune autonomie (tout est acheté)................6
      RaisonsRecensement__2 == 1 & eval(parse(text = autonomie_6_)) ~ 6,
      # Plus de 75 %
      RaisonsRecensement__2 == 1 & eval(parse(text = autonomie_1_2_)) ~ 1.5,
      # De 50 à 90%
      RaisonsRecensement__2 == 1 & eval(parse(text = autonomie_2_3_)) ~ 2.5,
      # De 25 à 75%
      RaisonsRecensement__2 == 1 & eval(parse(text = autonomie_3_4_)) ~ 3.5,
      # De 1 à 50%
      RaisonsRecensement__2 == 1 & eval(parse(text = autonomie_4_5_)) ~ 4.5,
      # De 0 à 25%
      RaisonsRecensement__2 == 1 & eval(parse(text = autonomie_5_6_)) ~ 5.5,
      TRUE ~ 999
    )
  )

autonomieNonCalculee <- rga23_prodAnimales_alimentation |> filter(niveauAutonomie == 999) |>
  select(
    "AutAlimAnimauxBasseCour", "AutAlimBovinsFourrage", "AutAlimCaprinsFourrage",
    "AutAlimEquidesFourrages", "AutAlimOvinsFourrage", "AutAlimPorcins", "AutAlimPoules"
  )
