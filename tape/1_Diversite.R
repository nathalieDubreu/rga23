# CULTURES
# > 0 - Monoculture (ou aucune culture).
# > 1 - Une culture couvrant plus de 80 pour cent de la surface cultivée.
# > 2 - Deux ou trois cultures avec une superficie cultivée importante.
# > 3 - Plus de 3 cultures avec une superficie cultivée importante adaptée aux conditions climatiques locales et changeantes.
# > 4 - Plus de 3 cultures de variétés différentes adaptées aux conditions locales et ferme spatialement diversifiée avec multi, poly- ou interculture.

rga23_surfacesCultures <- inner_join(
  inner_join(
    rga23_surfacesCultures,
    rga23_prodVegetales |> select(interview__key, ModesProduction__4, SurfaceTotalProdAgri, SurfaceJardins)
  ),
  rga23_tape |> select(interview__key, PratiquesCulturales__2)
) |>
  mutate(partCulture = SurfaceCult / SurfaceTotalProdAgri * 100)

nbCulturesDeclarees <- rga23_surfacesCultures |>
  count(interview__key, name = "nbCultures")

scoreCultures <- left_join(rga23_surfacesCultures, nbCulturesDeclarees) |>
  mutate(score = case_when(
    RaisonsRecensement__1 == 0 ~ 9,
    (nbCultures > 3 & PratiquesCulturales__2 == 1) | ModesProduction__4 == 1 ~ 4,
    nbCultures == 1 ~ 0,
    partCulture > 80 ~ 1,
    nbCultures == 2 | nbCultures == 3 ~ 2,
    nbCultures > 3 ~ 3
  )) |>
  group_by(interview__key, SurfaceTotalProdAgri, SurfaceJardins, nbCultures) |>
  summarize(score = min(score))

scoreCultures |>
  group_by(score) |>
  count()

# ANIMAUX (Y COMPRIS LES POISSONS ET LES INSECTES)
# > 0 - Aucun animal élevé.
# > 1 - Une seule espèce élevée.
# > 2 - Deux ou trois espèces, avec peu d’animaux.
# > 3 – Plus de 3 espèces avec un nombre significatif d’animaux.
# > 4 - Plus de 3 espèces de races différentes bien adaptées aux conditions climatiques locales et changeantes.

presenceEspecesAnimaux <- paste0("PresenceAnimaux__", 1:8)

rga23_prodAnimales <- rga23_prodAnimales %>%
  mutate(nbEspeces = rowSums(across(
    all_of(presenceEspecesAnimaux),
    ~ coalesce(., 0)
  )))

scoreAnimaux <- left_join(rga23_prodAnimales, rga23_gestion |> select(interview__key, indicRGA23_Elevage)) |>
  mutate(score = case_when(
    nbEspeces == 0 ~ 0,
    nbEspeces == 1 ~ 1,
    nbEspeces == 3 & indicRGA23_Elevage == 1 ~ 3,
    nbEspeces == 2 | nbEspeces == 3 ~ 2,
    nbEspeces > 3 ~ 3
  ))

scoreAnimaux |>
  group_by(score) |>
  count()

# ARBRES (ET AUTRES VIVACES)
# > 0 - Pas d’arbres (ni d’autres plantes vivaces).
# > 1 - Peu d’arbres (et / ou d’autres plantes vivaces) d’une seule.
# > 2 - Certains arbres (et / ou autres plantes vivaces) de plus d’une espèce.
# > 3 - Nombre important d’arbres (et / ou autres vivaces) d’espèces différentes.
# > 4 - Nombre élevé d’arbres (et / ou autres plantes vivaces) de différentes espèces intégrées dans les terres agricoles.

rga23_tape |>
  mutate(PresenceArbre = case_when(
    PsceArbresHorsRente__1 == 1 & PsceArbresHorsRente__2 == 1 ~ "1 - Présents en bord de parcelle et dans la parcelle",
    PsceArbresHorsRente__1 == 1 ~ "2 - Présents en bord de parcelle",
    PsceArbresHorsRente__2 == 1 ~ "3 - Présents dans la parcelle",
    PsceArbresHorsRente__3 == 1 ~ "4 - Absents",
    TRUE ~ "Non concernés"
  )) |>
  group_by(PresenceArbre) |>
  count()


# DIVERSITÉ DES ACTIVITÉS, PRODUITS ET SERVICES
# > 0 - Une seule activité productive (par ex. vente d’une seule culture).
# > 1 - Deux ou trois activités productives (par exemple, vendre 2 cultures ou une culture et un type d’animal).
# > 2 - Plus de 3 activités productives.
# > 3 - Plus de 3 activités productives et un service (par ex. transformation de produits à la ferme, écotourisme, transport de produits agricoles, formation, etc.).
# > 4 - Plus de 3 activités productives et plusieurs services.
