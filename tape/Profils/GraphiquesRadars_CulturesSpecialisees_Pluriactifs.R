library("tinytex")

# Etape 1 : décrire les profils et créér la variable utilisée pour grouper les exploitations (variable de profil)
descriptionProfils <- "**6 profils distincts :**\\
- Cultures - élevage : exploitations qui font de la culture ET de l'élevage\\
- Maj fruitiers : Cultures fruitières majoritaires i.e. sur au moins 2/3 de la SAU déclarée et aucun élevage\\
- Maj maraîchers : idem pour ces cultures\\
- Maj plantes aromatiques, stimulantes et médicinales : idem pour ces cultures\\
- Maj vivriers : idem pour ces cultures\\
- Cultures : Autres exploitations qui font de la culture ET pas d'élevage\\
\\
**Les exploitations qui ne font pas de cultures sont exclues de l'analyse.**"

rga23_profil <- left_join(
  rga23_general |>
    filter(RaisonsRecensement__1 == 1),
  rga23_prodVegetales |> select(interview__key, SurfaceTotalProdAgri, totalSurfaceMarai, totalSurfaceFruit, totalSurfaceVivri, totalSurfacePlantes),
  by = "interview__key"
) |>
  left_join(
    rga23_prodAnimales |> mutate(
      PresenceAnimauxPleinAir = case_when(
        # Accès au plein air pour les bovins
        PresenceAnimaux__1 == 1 & (BovinsPleinAir == 1 | BovinsPleinAir == 3) ~ 1,
        # Accès au plein air pour les caprins
        PresenceAnimaux__8 == 1 & (CaprinsPleinAir == 1 | CaprinsPleinAir == 3) ~ 1,
        # Accès au plein air pour les ovins
        PresenceAnimaux__2 == 1 & (OvinsPleinAir == 1 | OvinsPleinAir == 3) ~ 1,
        # Accès à un parcours pour les porcins
        PresenceAnimaux__3 == 1 & (AccesParcoursPorcins == 1 | AccesParcoursPorcins == 3) ~ 1,
        # Surface de parcours pour les poules pondeuses plein air ou sol supérieur à 0
        TypeVolailles__3 == 1 & (SurfaceParcBatSol1 > 0 | SurfaceParcCabMob1 > 0 | SurfaceParcAutreBat1 > 0) ~ 1,
        # Surface de parcours pour les poules biologiques supérieur à 0
        TypeVolailles__4 == 1 & (SurfaceParcBatSol0 > 0 | SurfaceParcCabMob0 > 0 | SurfaceParcAutreBat0 > 0) ~ 1,
        # Accès à un parcours extérieur pour les volailles hors poules pondeuses
        (TypeVolailles__5 == 1 | TypeVolailles__6 == 1 | TypeVolailles__7 == 1 | TypeVolailles__8 == 1 | TypeVolailles__9 == 1 | TypeVolailles__10 == 1 | TypeVolailles__11 == 1 | TypeVolailles__12 == 1 | TypeVolailles__13 == 1) &
          (AccesParcoursVolailles == 1 | AccesParcoursVolailles == 3) ~ 1,
        # Accès au plein air pour les équidés
        PresenceAnimaux__5 == 1 & (EquidesPleinAir == 1 | EquidesPleinAir == 3) ~ 1,
        TRUE ~ 0
      )
    ) |>
      select(interview__key, PresenceAnimauxPleinAir),
    by = "interview__key"
  ) |>
  mutate(Profil = case_when(
    RaisonsRecensement__2 == 0 & totalSurfaceMarai / SurfaceTotalProdAgri >= 2 / 3 ~ "Maj maraîchers",
    RaisonsRecensement__2 == 0 & totalSurfaceVivri / SurfaceTotalProdAgri >= 2 / 3 ~ "Maj vivriers",
    RaisonsRecensement__2 == 0 & totalSurfaceFruit / SurfaceTotalProdAgri >= 2 / 3 ~ "Maj fruitiers",
    RaisonsRecensement__2 == 0 & totalSurfacePlantes / SurfaceTotalProdAgri >= 2 / 3 ~ "Maj plantes aromatiques, stimulantes et médicinales",
    RaisonsRecensement__2 == 1 ~ "Cultures + élevage",
    RaisonsRecensement__2 == 0 ~ "Cultures seules",
    TRUE ~ "AUTRE"
  )) |>
  select(interview__key, Profil)

# Lancement des programmes communs quelque soit la variable de profil
source("tape/Profils/FonctionsGraphiques.R")
source("tape/Profils/PreparationTablesGraphiquesRadar.R")

# Etape 2 :
titreDansLeDocument <- "Graphiques Radars par type d'exploitations (cultures spécialisées vs. pluriactifs - y compris élevage)"
nomFichierPDF <- "GraphiquesRadars_ByCultSpecOuPluri.pdf"

# Création du fichier contenant les graphiques
rmarkdown::render(
  input = "tape/Profils/GraphiquesRadars.Rmd",
  output_file = nomFichierPDF
)
