# CULTURES
# > 0 - Monoculture (ou aucune culture).
# > 1 - Une culture couvrant plus de 80 pour cent de la surface cultivée.
# > 2 - Deux ou trois cultures avec une superficie cultivée importante.
# > 3 - Plus de 3 cultures avec une superficie cultivée importante adaptée aux conditions climatiques locales et changeantes.
# > 4 - Plus de 3 cultures de variétés différentes adaptées aux conditions locales et ferme spatialement diversifiée avec multi, poly- ou interculture.

rga23_surfacesCultures_avecPratiques <- inner_join(
  full_join(
    rga23_surfacesCultures,
    rga23_prodVegetales |> select(interview__key, ModesProduction__4, SurfaceTotalProdAgri, SurfaceJardins),
    by = "interview__key"
  ),
  rga23_tape |> select(interview__key, PratiquesCulturales__2),
  by = "interview__key"
) |>
  mutate(partCulture = SurfaceCult / SurfaceTotalProdAgri * 100)

nbCulturesDeclarees <- rga23_surfacesCultures_avecPratiques |>
  count(interview__key, name = "nbCultures")

score_1_Cultures <- left_join(rga23_surfacesCultures_avecPratiques, nbCulturesDeclarees, by = "interview__key") |>
  mutate(score = case_when(
    RaisonsRecensement__1 == 0 ~ 0,
    (nbCultures > 3 & PratiquesCulturales__2 == 1) | ModesProduction__4 == 1 ~ 4,
    nbCultures == 1 ~ 0,
    partCulture > 80 ~ 1,
    nbCultures == 2 | nbCultures == 3 ~ 2,
    nbCultures > 3 ~ 3
  )) |>
  group_by(interview__key, SurfaceTotalProdAgri, SurfaceJardins, nbCultures) |>
  summarize(score = min(score))

score_1_Cultures |>
  group_by(score) |>
  count()

# ANIMAUX (Y COMPRIS LES POISSONS ET LES INSECTES)
# > 0 - Aucun animal élevé.
# > 1 - Une seule espèce élevée.
# > 2 - Deux ou trois espèces, avec peu d’animaux.
# > 3 – Plus de 3 espèces avec un nombre significatif d’animaux.
# > 4 - Plus de 3 espèces de races différentes bien adaptées aux conditions climatiques locales et changeantes.

score_2_Animaux <- left_join(rga23_prodAnimales, rga23_general |> select(interview__key, indicRGA23_Elevage), by = "interview__key") |>
  mutate(score = case_when(
    nbEspeces == 0 ~ 0,
    nbEspeces == 1 ~ 1,
    nbEspeces == 3 & indicRGA23_Elevage == 1 ~ 3,
    nbEspeces == 2 | nbEspeces == 3 ~ 2,
    nbEspeces > 3 ~ 3
  ))

score_2_Animaux |>
  group_by(score) |>
  count()

# ARBRES (ET AUTRES VIVACES)
# > 0 - Pas d’arbres (ni d’autres plantes vivaces).
# > 1 - Peu d’arbres (et / ou d’autres plantes vivaces) d’une seule.
# > 2 - Certains arbres (et / ou autres plantes vivaces) de plus d’une espèce.
# > 3 - Nombre important d’arbres (et / ou autres vivaces) d’espèces différentes.
# > 4 - Nombre élevé d’arbres (et / ou autres plantes vivaces) de différentes espèces intégrées dans les terres agricoles.

rga23_tapeAvecPresenceArbres <- rga23_tape |>
  mutate(PresenceArbre = case_when(
    PsceArbresHorsRente__1 == 1 & PsceArbresHorsRente__2 == 1 ~ "1 - Présents en bord de parcelle et dans la parcelle",
    PsceArbresHorsRente__1 == 1 ~ "2 - Présents en bord de parcelle",
    PsceArbresHorsRente__2 == 1 ~ "3 - Présents dans la parcelle",
    PsceArbresHorsRente__3 == 1 ~ "4 - Absents",
    TRUE ~ "Non concernés"
  ))

jointuresArbres <- left_join(
  rga23_tapeAvecPresenceArbres |> select(interview__key, RaisonsRecensement__1, PsceArbresHorsRente__1, PsceArbresHorsRente__2, PsceArbresHorsRente__3),
  nbCulturesArbresDeclarees,
  by = "interview__key"
) |>
  left_join(
    rga23_prodVegetales |> select(interview__key, ModesProduction__4, SurfaceTotalProdAgri),
    by = "interview__key"
  ) |>
  mutate(
    NbArbresHectares = 10000 / (SurfaceTotalProdAgri / nbPiedsTotal),
    PartSurfaceArbres = surfaceTotalArbres / SurfaceTotalProdAgri * 100
  )

score_3_Arbres <- jointuresArbres |> mutate(score = case_when(
  # PREMIER SET DE CONDITIONS
  # Absence de cultures du tout + Absence d'arbres hors rente
  RaisonsRecensement__1 == 0 & (PsceArbresHorsRente__3 == 1 | is.na(PsceArbresHorsRente__3)) ~ 0,
  # Absence de cultures d'arbres + Absence d'arbres hors rente + Absence jardins océaniens
  is.na(nbCulturesArbres) & (PsceArbresHorsRente__3 == 1 | is.na(PsceArbresHorsRente__3)) & (ModesProduction__4 == 0 | is.na(ModesProduction__4)) ~ 0,
  # Arbres hors rentes mais aucune culture d'arbres
  ((PsceArbresHorsRente__1 == 1 | PsceArbresHorsRente__2 == 1)) & is.na(nbCulturesArbres) & (ModesProduction__4 == 0 | is.na(ModesProduction__4)) ~ 1,
  # Plus d'une culture d'arbres + <3 arbres à l'hectare + Absence jardins océaniens
  PsceArbresHorsRente__3 == 1 & nbCulturesArbres == 1 & nombrePiedsConnu & NbArbresHectares < 3 & ModesProduction__4 == 0 ~ 1,
  # Plus d'une culture d'arbres : De 3 et 10 arbres à l'Ha + pas de jardins océaniens
  ((nbCulturesArbres == 1 & (PsceArbresHorsRente__1 == 1 | PsceArbresHorsRente__2 == 1)) | nbCulturesArbres > 1) & nombrePiedsConnu & NbArbresHectares >= 3 & NbArbresHectares <= 10 & ModesProduction__4 == 0 ~ 2,
  # Plus d'une culture d'arbres : De 10 à < 30 arbres à l'Ha + pas de jardins océaniens
  ((nbCulturesArbres == 1 & (PsceArbresHorsRente__1 == 1 | PsceArbresHorsRente__2 == 1)) | nbCulturesArbres > 1) & nombrePiedsConnu & NbArbresHectares > 10 & NbArbresHectares <= 30 & ModesProduction__4 == 0 ~ 3,
  # Au moins une culture d'arbres : > 30 arbres à l'Ha OU présence de jardins océaniens
  (nbCulturesArbres >= 1 & nombrePiedsConnu & NbArbresHectares > 30) | ModesProduction__4 == 1 ~ 4,
  # DEUXIEME SET DE CONDITIONS
  # Moins de 3% de la surface cultivée est occupée par des cultures d'arbres
  PartSurfaceArbres < 3 ~ 1,
  # De 3% à 10 % de la surface cultivée est occupée par des cultures d'arbres
  PartSurfaceArbres <= 10 ~ 2,
  # De 10% à 30 % de la surface cultivée est occupée par des cultures d'arbres
  PartSurfaceArbres <= 30 ~ 3,
  # Plus de 30 % de la surface cultivée est occupée par des cultures d'arbres
  PartSurfaceArbres > 30 ~ 4,
  TRUE ~ 55
))

score_3_Arbres |>
  group_by(score) |>
  count()

# DIVERSITÉ DES ACTIVITÉS, PRODUITS ET SERVICES
# > 0 - Une seule activité productive (par ex. vente d’une seule culture).
# > 1 - Deux ou trois activités productives (par exemple, vendre 2 cultures ou une culture et un type d’animal).
# > 2 - Plus de 3 activités productives.
# > 3 - Plus de 3 activités productives et un service (par ex. transformation de produits à la ferme, écotourisme, transport de produits agricoles, formation, etc.).
# > 4 - Plus de 3 activités productives et plusieurs services.

transformationsPossibles <- paste0("TransformationPA__", 1:16)

typesCultHorsJacheres <- paste0("CulturesPresentes__", 1:7, "0")

score_4_Activites <- left_join(rga23_tapeAvecVentes,
  rga23_mainOeuvre |> mutate(
    nbTransformations = rowSums(across(
      all_of(transformationsPossibles),
      ~ coalesce(., 0)
    )),
    nbServices = nbTransformations + ActivitesChefExploit__7 + ActivitesChefExploit__8
  ) |>
    select(interview__key, nbTransformations, nbServices),
  by = "interview__key"
) |>
  left_join(
    rga23_prodVegetales |>
      mutate(
        nombreTypesCultHorsJacheres = rowSums(across(
          all_of(typesCultHorsJacheres),
          ~ coalesce(., 0)
        ))
      ) |>
      select(interview__key, ModesProduction__4, nombreTypesCultHorsJacheres),
    by = "interview__key"
  ) |>
  left_join(
    rga23_prodAnimales |>
      select(interview__key, nbEspeces),
    by = "interview__key"
  ) |>
  left_join(
    rga23_exploitations |>
      select(interview__key, ProductionAgricole),
    by = "interview__key"
  ) |>
  mutate(score = case_when(
    # 0 : Vente d'un seul produit
    venteNbProduits == 1 ~ 0,
    # 1 : vente de 2 ou 3 produits ou vente d'un seul type de produits mais Jardins océaniens
    venteNbProduits == 2 | venteNbProduits == 3 | (venteTypeProduits == 1 & ModesProduction__4 == 1) ~ 1,
    # > 2 - Plus de 3 activités productives mais pas de service
    (venteNbProduits > 3 | (venteTypeProduits > 1 & ModesProduction__4 == 1)) & nbServices == 0 ~ 2,
    # > 3 - Plus de 3 activités productives et un service (par ex. transformation de produits à la ferme, écotourisme, transport de produits agricoles, formation, etc.).
    (venteNbProduits > 3 | (venteTypeProduits > 1 & ModesProduction__4 == 1)) & nbServices == 1 ~ 3,
    # > 4 - Plus de 3 activités productives et plusieurs services.
    (venteNbProduits > 3 | (venteTypeProduits > 1 & ModesProduction__4 == 1)) & nbServices > 1 ~ 4,
    # Pas de vente de produit mais production d'un type de cultures végétales ou élevage
    ProductionAgricole == 1 & nombreTypesCultHorsJacheres + replace_na(nbEspeces, 0) == 1 ~ 0,
    # Pas de vente de produit mais production de 2 ou 3 types de cultures végétales ou élevage
    ProductionAgricole == 1 & (nombreTypesCultHorsJacheres + replace_na(nbEspeces, 0) == 2 | nombreTypesCultHorsJacheres + replace_na(nbEspeces, 0) == 3) ~ 1,
    # Pas de vente de produit mais production de plus de 3 types de cultures végétales ou élevage
    ProductionAgricole == 1 & (nombreTypesCultHorsJacheres + replace_na(nbEspeces, 0) > 3 | ModesProduction__4 == 1) ~ 2,
    # Non concernés : pas de production agricole
    ProductionAgricole == 2 ~ 99,
    TRUE ~ 55
  ))

score_4_Activites |>
  group_by(score) |>
  count()
