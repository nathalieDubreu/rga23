# Champ TAPE : Restriction aux exploitations ayant plus de 400 points CAPL
rga23_eligibles <- readCSV("rga23_general.csv") |>
  filter(PointsCAPL >= 400) |>
  select(interview__key, lettre_unite, RaisonsRecensement__1, RaisonsRecensement__2, RaisonsRecensement__3)

## Imports des fichiers utiles
rga23_tape <- left_join(rga23_eligibles, readCSV("rga23_tape.csv"), by = "interview__key")
rga23_prodVegetales <- left_join(rga23_eligibles, readCSV("rga23_prodVegetales.csv"), by = "interview__key")
rga23_prodAnimales <- left_join(rga23_eligibles, readCSV("rga23_prodAnimales.csv"), by = "interview__key")
rga23_exploitations <- left_join(rga23_eligibles, readCSV("rga23_exploitations.csv"), by = "interview__key")
rga23_surfacesCultures <- inner_join(rga23_eligibles, readCSV("rga23_surfacesCultures.csv"), by = "interview__key")
rga23_general <- left_join(rga23_eligibles |> select(interview__key), readCSV("rga23_general.csv"), by = "interview__key")
rga23_mainOeuvre <- left_join(rga23_eligibles, readCSV("rga23_mainOeuvre.csv"), by = "interview__key")

### arbres ou non
arbres <- readInputCSV("arbres.csv") |>
  select(culture_id, arbre)

culturesArbres <- left_join(rga23_surfacesCultures, arbres, by = "culture_id") |>
  filter(arbre == 1)

nbCulturesArbresDeclarees <- culturesArbres |>
  group_by(interview__key) |>
  summarize(
    nbCulturesArbres = n(),
    nombrePiedsConnu = all(!is.na(NbPieds)),
    nbPiedsTotal = sum(NbPieds),
    surfaceTotalArbres = sum(SurfaceCult)
  )

### Ajout du nombre d'espèces et du nombre d'espèces (hors abeilles) dans les productions animales
presenceEspecesAnimaux <- paste0("PresenceAnimaux__", 1:8)
rga23_prodAnimales <- rga23_prodAnimales |>
  mutate(
    nbEspeces = rowSums(across(
      all_of(presenceEspecesAnimaux),
      ~ coalesce(., 0)
    )),
    nbEspecesHorsAbeilles = nbEspeces - replace_na(PresenceAnimaux__7, 0)
  )

## Etapes préalables
source("tape/0_AlimentationAnimaux.R")
source("tape/0_VenteProduits.R")

## Catégories (peuvent être lancées séparément)
source("tape/1_Diversite.R")
source("tape/2_Synergies.R")
source("tape/3_Efficience.R")
source("tape/4_Recyclage.R")
source("tape/5_Resilience.R")
source("tape/6_CultureTraditions.R")
source("tape/7_Cocreation.R")
source("tape/8_ValeursHumainesSociales.R")
source("tape/9_EconomieCirculaire.R")
source("tape/10_Gouvernance.R")

source("tape/ClassementsExploitations.R")

source("tape/RecuperationScores.R")
