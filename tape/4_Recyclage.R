# 4. RECYCLAGE
#
# 4.1 RECYCLAGE DE LA BIOMASSE ET DES NUTRIENTS
# > 0 – Les résidus et sous-produits ne sont pas recyclés (par ex. laissés à se décomposer ou brûlés). Des grandes quantités de déchets sont déversés ou brûlées.
# > 1 - Une petite partie des résidus et sous-produits est recyclée (par ex. résidus de cultures comme aliments pour animaux, utilisation de fumier comme engrais, production de compost à partir de fumier et de déchets ménagers, engrais vert). Les déchets sont déversés ou brûlés.
# > 2 - Plus de la moitié des résidus et sous-produits sont recyclés. Certains déchets sont déversés ou brûlés.
# > 3 - La plupart des résidus et sous-produits sont recyclés. Seuls quelques déchets sont déversés ou brûlés.
# > 4 - Tous les résidus et sous-produits sont recyclés. Aucun déchet n’est déversé ou brûlé.
#
# 4.2 PRÉSERVATION ET CONSERVATION DE L’EAU
# > 0 - Aucune installation ni technique de préservation et conservation de l’eau.
# > 1 - Un type d’installation pour la collecte ou la préservation de l’eau (par ex. irrigation goutte à goutte, réservoir).
# > 2 - Un type d’installation pour la collecte ou la préservation de l’eau et utilisation d’une pratique pour limiter l’utilisation de l’eau (par ex. synchronisation de l’irrigation, cultures de couverture).
# > 3 - Un type d’installation pour la colelcte ou la préservation de l’eau et différentes pratiques pour limiter l’utilisation de l’eau.
# > 4 - Plusieurs types d’installations pour la collecte ou la préservation de l’eau et différentes pratiques pour limiter l’utilisation de l’eau.

# OrigineEauIrrig
# Réseau collectif agricole.....1
# Réseau individuel.............2
# Réseau collectif (communal)...3

# OrigineEauIndivIrrig - Réseau individuel - Origine de l'eau pour le prélèvement individuel ?
# Eaux de surface : cours d'eau, canaux, lacs, captages...................1
# Réservoirs d'eau non connectés à un cours d'eau, retenues collinaires...2
# Autre mode de récupération eaux de pluie................................3
# Eaux souterraines : forage, puits, lentille d'eau.......................4
# Traitement de l'eau salée...............................................5
# Autres origines.........................................................6

# ModeIrrigation
# Aspersion.....................................1
# Goutte à goutte...............................2
# Micro-asperseurs..............................3
# Manuellement (tuyau, cuve sur tracteur).......4
# Autres canaux d'irrigation (tarodière, ...)...5

# PratiquesCulturales : Paillage (plastique ou naturel)................5

# Arbres présents hors culture de rente pour l'ombrage : RaisonsArbresHorsR__3

score_2_Eau <- left_join(rga23_tape,
  rga23_prodVegetales |> select(interview__key, PresSurfIrrigables, OrigineEauIrrig__1, OrigineEauIrrig__2, OrigineEauIrrig__3, OrigineEauIndivIrrig__2, OrigineEauIndivIrrig__3, OrigineEauIndivIrrig__4, OrigineEauIndivIrrig__5, ModeIrrigation__2, ModeIrrigation__3),
  by = "interview__key"
) |>
  mutate(
    nbTypesInstallations =
      ifelse(is.na(OrigineEauIndivIrrig__2) | OrigineEauIndivIrrig__2 == 0, 0, 1) +
        ifelse(is.na(OrigineEauIndivIrrig__3) | OrigineEauIndivIrrig__3 == 0, 0, 1) +
        ifelse(is.na(OrigineEauIndivIrrig__4) | OrigineEauIndivIrrig__4 == 0, 0, 1) +
        ifelse(is.na(OrigineEauIndivIrrig__5) | OrigineEauIndivIrrig__5 == 0, 0, 1) +
        ifelse(OrigineEauIrrig__1 == 0 & OrigineEauIrrig__2 == 0 & OrigineEauIrrig__3 == 1, 1, 0) +
        ifelse(OrigineEauIrrig__1 == 1 & OrigineEauIrrig__2 == 0 & OrigineEauIrrig__3 == 0, 1, 0)
  ) |>
  mutate(score = case_when(
    # Pas de surfaces irrigables OU aucune installation de la liste, ni Paillage ni Goutte à goutte ni Micro-asperseurs
    PresSurfIrrigables == 2 ~ 0,
    nbTypesInstallations == 0 & replace_na(PratiquesCulturales__5, 0) == 0 & replace_na(ModeIrrigation__2, 0) == 0 & replace_na(ModeIrrigation__3, 0) == 0 ~ 0,
    # 1 seul type d'installation parmi la liste précédente + ni Paillage ni Goutte à goutte ni Micro-asperseurs OU aucun type d'installation mais une seule de ces 3 techniques de préservation
    nbTypesInstallations == 1 & replace_na(PratiquesCulturales__5, 0) == 0 & replace_na(ModeIrrigation__2, 0) == 0 & replace_na(ModeIrrigation__3, 0) == 0 ~ 1,
    nbTypesInstallations == 0 & (replace_na(PratiquesCulturales__5, 0) + replace_na(ModeIrrigation__2, 0) + replace_na(ModeIrrigation__3, 0)) == 1 ~ 1,
    # 1 seul type d'installation parmi la liste précédente + soit Paillage soit Goutte à goutte soit Micro-asperseurs (1 seule de ces 3 techniques)
    nbTypesInstallations == 1 & (replace_na(PratiquesCulturales__5, 0) + replace_na(ModeIrrigation__2, 0) + replace_na(ModeIrrigation__3, 0)) == 1 ~ 2,
    # 1 seul type d'installation parmi la liste précédente + Paillage et/ou Goutte à goutte et/ou Micro-asperseurs et/ou arbres hors rente utilisés pour l'ombrage (2 de ces 4 techniques)
    nbTypesInstallations == 1 & (replace_na(PratiquesCulturales__5, 0) + replace_na(ModeIrrigation__2, 0) + replace_na(ModeIrrigation__3, 0) + replace_na(RaisonsArbresHorsR__3, 0)) == 2 ~ 3,
    # Au moins 2 types d'installation parmi la liste précédente + Paillage et/ou Goutte à goutte et/ou Micro-asperseurs et/ou arbres hors rente utilisés pour l'ombrage (min 2 de ces 4 techniques)
    nbTypesInstallations > 1 & (replace_na(PratiquesCulturales__5, 0) + replace_na(ModeIrrigation__2, 0) + replace_na(ModeIrrigation__3, 0) + replace_na(RaisonsArbresHorsR__3, 0)) >= 2 ~ 4,
    # Pas de culture -> non concernés
    RaisonsRecensement__1 == 0 ~ 99,
    TRUE ~ 55
  ))

score_2_Eau |>
  group_by(score) |>
  count()

score_2_Eau |>
  filter(score == 55) |>
  group_by(nbTypesInstallations) |>
  count()

# 4.3 GESTION DES GRAINES ET DES RACES ANIMALES
# > 0 - Toutes les graines et/ou les ressources génétiques animales (par ex. poussins, jeunes animaux, sperme) sont achetées sur le marché.
# > 1 - Plus de 80 pour cent des graines/ressources génétiques animales sont achetées sur le marché.
# > 2 - Environ la moitié des graines sont autoproduites ou échangées, l’autre moitié est achetée au marché. Environ la moitié de l’élevage se fait dans les fermes voisines.
# > 3 - La majorité des graines/ressources génétiques animales sont autoproduites ou échangées. Certaines graines spécifiques sont achetées sur le marché.
# > 4 - Toutes les graines/ressources génétiques animales sont autoproduites, échangées avec d’autres agriculteurs ou gérées collectivement, assurant suffisamment de renouvellement et de diversité.
#
# 4.4 ENERGIE RENOUVELABLE (UTILISATION ET PRODUCTION)
# > 0 - Aucune énergie renouvelable est utilisée ou produite.
# > 1 - La grande partie de l’énergie provient de l’extérieur, mais une partie est produite dans l’agroécosystème (traction animale, vent, eau, biogaz, etc.).
# > 2 - La moitié de l’énergie utilisée est autoproduite, l’autre moitié est achetée.
# > 3 - Production importante d’énergie renouvelable, utilisation négligeable de carburant et d’autres sources non renouvelables.
# > 4 - Toute l’énergie utilisée est renouvelable et/ou autoproduite. Le ménage est autosuffisant pour l’approvisionnement en énergie, qui est garanti à tout moment. L’utilisation de combustibles fossiles est négligeable.

score_4_Energie <- rga23_tape |>
  mutate(score = case_when(
    EnergiesRenouv == 2 ~ 0,
    NivAutoEnergiesR == 1 ~ 1,
    NivAutoEnergiesR == 2 ~ 2,
    NivAutoEnergiesR == 3 ~ 3,
    NivAutoEnergiesR == 4 ~ 4,
    TRUE ~ 55
  ))

score_4_Energie |>
  group_by(score) |>
  count()
