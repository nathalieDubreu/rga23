# 2. SYNERGIES
#
# INTÉGRATION CULTURES-ÉLEVAGE-AQUACULTURE
# L’énumérateur doit tenir compte des ressources partagées au niveau communautaire. Dans le cas des pâturages communaux par exemple, les intrants alimentaires correspondants pour les animaux ne sont pas considérés comme externes. Seuls les aliments achetés sur le marché sont considérés comme externes.
# > 0 - Pas d’intégration: les animaux, y compris les poissons, sont nourris avec des aliments achetés
# et leur fumier n’est pas utilisé pour la fertilité du sol; OU il n’y a pas d’animaux dans l’agroécosystème.
# > 1 - Faible intégration: les animaux sont principalement nourris avec des aliments achetés,
# leur fumier est utilisé comme engrais.
# > 2 - Intégration moyenne: les animaux sont principalement nourris avec des aliments produits à la ferme et/ou au pâturage,
# leur fumier est utilisé comme engrais.
# > 3 - Intégration élevée: les animaux sont principalement nourris avec des aliments produits à la ferme, des résidus de récolte et des sous-produits et / ou des pâturages,
# leur fumier est utilisé comme engrais et ils assurent un service (par ex. la traction).
# > 4 - Intégration complète: les animaux sont exclusivement nourris avec des aliments produits à la ferme, des résidus de récolte et des sous-produits et / ou des pâturages,
# tout leur fumier est recyclé comme engrais et ils fournissent plus d’un service (par ex. nourriture, produits, traction, etc.).

rga23_prodAnimales_NbEtPoids <- rga23_prodAnimales_alimentation |>
  mutate(
    nbEspecesRuminants = rowSums(across(
      c(PresenceAnimaux__1, PresenceAnimaux__2, PresenceAnimaux__5, PresenceAnimaux__8),
      ~ coalesce(., 0)
    )),
    uniquementRuminants = case_when(
      nbEspecesRuminants == nbEspecesHorsAbeilles ~ 1,
      TRUE ~ 0
    ),
    nbAnimauxBasseCour = rowSums(across(
      c("NbOies", "NbCanards", "NbCailles", "NbPintades", "NbPouletsChairCoqs", "NbPoulettes", "NbPoussins", "NbLapereaux", "NbLapinesFutures", "NbLapinesMeres", "NbLapinsReprod", "NbLapinsSevresEngrais"),
      ~ coalesce(., 0)
    )),
    nbPoulesPondeuses = rowSums(across(
      c("NombrePoules0", "NombrePoules1", "NombrePoules3"),
      ~ coalesce(., 0)
    )),
    nbTotalAnimaux = rowSums(across(
      c("nbAnimauxBasseCour", "nbPoulesPondeuses", "nbTotalBovins", "nbTotalOvins", "nbTotalPorcs", "nbTotalEquides", "nbTotalCaprins"),
      ~ coalesce(., 0)
    )),
    poidsBovins = 900 * replace_na(nbTotalBovins, 0),
    poidsOvins = 150 * replace_na(nbTotalOvins, 0),
    poidsPoules = 3 * replace_na(nbPoulesPondeuses, 0),
    poidsAnimauxBasseCour = 3 * replace_na(nbAnimauxBasseCour, 0),
    poidsCaprins = 150 * replace_na(nbTotalCaprins, 0),
    poidsEquides = 800 * replace_na(nbTotalEquides, 0),
    poidsPorcins = 150 * replace_na(nbTotalPorcs, 0),
    poidsMax = pmax(poidsBovins, poidsOvins, poidsPoules, poidsAnimauxBasseCour, poidsCaprins, poidsEquides, poidsPorcins)
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

score_1_Integration <- left_join(rga23_exploitations, rga23_prodAnimales_NbEtPoids, by = c("interview__key")) |>
  mutate(score = case_when(
    nbEspecesHorsAbeilles == 0 ~ 0,
    (is.na(niveauAutonomie) | niveauAutonomie == 6) &
      PropRecyclEngraisOrga == 1 ~ 0,
    (is.na(niveauAutonomie) | niveauAutonomie == 4 | niveauAutonomie == 5 | niveauAutonomie == 4.5) &
      (PropRecyclEngraisOrga == 1 | PropRecyclEngraisOrga == 2) ~ 1,
    (is.na(niveauAutonomie) | niveauAutonomie == 3) &
      (PropRecyclEngraisOrga == 2 | PropRecyclEngraisOrga == 3) ~ 2,
    (is.na(niveauAutonomie) | niveauAutonomie == 2) &
      (PropRecyclEngraisOrga == 3 | PropRecyclEngraisOrga == 4) ~ 3,
    (is.na(niveauAutonomie) | niveauAutonomie == 1) &
      (PropRecyclEngraisOrga == 4) ~ 4,
    ## Cas des élevages avec uniquement des ruminants (hors abeilles)
    (uniquementRuminants == 1 & PropRecyclEngraisOrga == 1) ~ 1,
    (uniquementRuminants == 1 & PropRecyclEngraisOrga == 2) ~ 2,
    (uniquementRuminants == 1 & PropRecyclEngraisOrga == 3) ~ 3,
    (uniquementRuminants == 1 & PropRecyclEngraisOrga == 4) ~ 4,
    ## Cas des élevages avec uniquement des poules pondeuses (hors abeilles)
    (nbEspecesHorsAbeilles == 1 & nbPoulesPondeuses > 0 & PropRecyclEngraisOrga == 1) ~ 0,
    (nbEspecesHorsAbeilles == 1 & nbPoulesPondeuses > 0 & PropRecyclEngraisOrga == 2) ~ 1,
    (nbEspecesHorsAbeilles == 1 & nbPoulesPondeuses > 0 & PropRecyclEngraisOrga == 3) ~ 2,
    (nbEspecesHorsAbeilles == 1 & nbPoulesPondeuses > 0 & PropRecyclEngraisOrga == 4) ~ 3,
    ## Cas des élevages avec uniquement 5 porcins ou moins (hors abeilles)
    (nbEspecesHorsAbeilles == 1 & nbTotalPorcs > 0 & nbTotalPorcs <= 5 & AutAlimPorcins == 6) ~ 0,
    (nbEspecesHorsAbeilles == 1 & nbTotalPorcs > 0 & nbTotalPorcs <= 5 & (AutAlimPorcins == 4 | AutAlimPorcins == 5)) ~ 1,
    (nbEspecesHorsAbeilles == 1 & nbTotalPorcs > 0 & nbTotalPorcs <= 5 & AutAlimPorcins == 3) ~ 2,
    (nbEspecesHorsAbeilles == 1 & nbTotalPorcs > 0 & nbTotalPorcs <= 5 & AutAlimPorcins == 2) ~ 3,
    (nbEspecesHorsAbeilles == 1 & nbTotalPorcs > 0 & nbTotalPorcs <= 5 & AutAlimPorcins == 1) ~ 4,
    ## Prise en compte uniquement de l'espèce avec le poids UGB le plus élevé
    ((is.na(AutAlimAnimauxBasseCour) | AutAlimAnimauxBasseCour == 6 | poidsAnimauxBasseCour < poidsMax) &
      (is.na(AutAlimBovinsFourrage) | AutAlimBovinsFourrage == 6 | poidsBovins < poidsMax) &
      (is.na(AutAlimCaprinsFourrage) | AutAlimCaprinsFourrage == 6 | poidsCaprins < poidsMax) &
      (is.na(AutAlimEquidesFourrages) | AutAlimEquidesFourrages == 6 | poidsEquides < poidsMax) &
      (is.na(AutAlimOvinsFourrage) | AutAlimOvinsFourrage == 6 | poidsOvins < poidsMax) &
      (is.na(AutAlimPorcins) | AutAlimPorcins == 6 | poidsPorcins < poidsMax) &
      (is.na(AutAlimPoules) | AutAlimPoules == 6 | poidsPoules < poidsMax)) &
      PropRecyclEngraisOrga == 1 ~ 0,
    ((is.na(AutAlimAnimauxBasseCour) | AutAlimAnimauxBasseCour == 5 | AutAlimAnimauxBasseCour == 4 | poidsAnimauxBasseCour < poidsMax) &
      (is.na(AutAlimBovinsFourrage) | AutAlimBovinsFourrage == 5 | AutAlimBovinsFourrage == 4 | poidsBovins < poidsMax) &
      (is.na(AutAlimCaprinsFourrage) | AutAlimCaprinsFourrage == 5 | AutAlimCaprinsFourrage == 4 | poidsCaprins < poidsMax) &
      (is.na(AutAlimEquidesFourrages) | AutAlimEquidesFourrages == 5 | AutAlimEquidesFourrages == 4 | poidsEquides < poidsMax) &
      (is.na(AutAlimOvinsFourrage) | AutAlimOvinsFourrage == 5 | AutAlimOvinsFourrage == 4 | poidsOvins < poidsMax) &
      (is.na(AutAlimPorcins) | AutAlimPorcins == 5 | AutAlimPorcins == 4 | poidsPorcins < poidsMax) &
      (is.na(AutAlimPoules) | AutAlimPoules == 5 | AutAlimPoules == 4 | poidsPoules < poidsMax)) &
      (PropRecyclEngraisOrga == 1 | PropRecyclEngraisOrga == 2) ~ 1,
    ((is.na(AutAlimAnimauxBasseCour) | AutAlimAnimauxBasseCour == 3 | poidsAnimauxBasseCour < poidsMax) &
      (is.na(AutAlimBovinsFourrage) | AutAlimBovinsFourrage == 3 | poidsBovins < poidsMax) &
      (is.na(AutAlimCaprinsFourrage) | AutAlimCaprinsFourrage == 3 | poidsCaprins < poidsMax) &
      (is.na(AutAlimEquidesFourrages) | AutAlimEquidesFourrages == 3 | poidsEquides < poidsMax) &
      (is.na(AutAlimOvinsFourrage) | AutAlimOvinsFourrage == 3 | poidsOvins < poidsMax) &
      (is.na(AutAlimPorcins) | AutAlimPorcins == 3 | poidsPorcins < poidsMax) &
      (is.na(AutAlimPoules) | AutAlimPoules == 3 | poidsPoules < poidsMax)) &
      (PropRecyclEngraisOrga == 2 | PropRecyclEngraisOrga == 3) ~ 2,
    ((is.na(AutAlimAnimauxBasseCour) | AutAlimAnimauxBasseCour == 2 | poidsAnimauxBasseCour < poidsMax) &
      (is.na(AutAlimBovinsFourrage) | AutAlimBovinsFourrage == 2 | poidsBovins < poidsMax) &
      (is.na(AutAlimCaprinsFourrage) | AutAlimCaprinsFourrage == 2 | poidsCaprins < poidsMax) &
      (is.na(AutAlimEquidesFourrages) | AutAlimEquidesFourrages == 2 | poidsEquides < poidsMax) &
      (is.na(AutAlimOvinsFourrage) | AutAlimOvinsFourrage == 2 | poidsOvins < poidsMax) &
      (is.na(AutAlimPorcins) | AutAlimPorcins == 2 | poidsPorcins < poidsMax) &
      (is.na(AutAlimPoules) | AutAlimPoules == 2 | poidsPoules < poidsMax)) &
      (PropRecyclEngraisOrga == 3 | PropRecyclEngraisOrga == 4) ~ 3,
    ((is.na(AutAlimAnimauxBasseCour) | AutAlimAnimauxBasseCour == 1 | poidsAnimauxBasseCour < poidsMax) &
      (is.na(AutAlimBovinsFourrage) | AutAlimBovinsFourrage == 1 | poidsBovins < poidsMax) &
      (is.na(AutAlimCaprinsFourrage) | AutAlimCaprinsFourrage == 1 | poidsCaprins < poidsMax) &
      (is.na(AutAlimEquidesFourrages) | AutAlimEquidesFourrages == 1 | poidsEquides < poidsMax) &
      (is.na(AutAlimOvinsFourrage) | AutAlimOvinsFourrage == 1 | poidsOvins < poidsMax) &
      (is.na(AutAlimPorcins) | AutAlimPorcins == 1 | poidsPorcins < poidsMax) &
      (is.na(AutAlimPoules) | AutAlimPoules == 1 | poidsPoules < poidsMax)) &
      (PropRecyclEngraisOrga == 4) ~ 4,
    TRUE ~ 55
  ))

score_1_Integration |>
  group_by(score) |>
  count()

# PropRecyclEngraisOrga

# GESTION DU SYSTÈME SOL-PLANTES
# > 0 - Le sol est nu après la récolte. Pas de culture intercalaire. Aucune rotation des cultures (ou systèmes de pâturage en rotation). Perturbation importante du sol (biologique, chimique ou mécanique).
# > 1 - Moins de 20 pour cent des terres arables sont couvertes de résidus ou de cultures de couverture. Plus de 80 pour cent des cultures sont produites en monoculture continue (ou sans pâturage en rotation).
# > 2 - 50 pour cent du sol est recouvert de résidus ou de cultures de couverture. Certaines cultures sont tournées ou intercalées (ou un pâturage en rotation est effectué).
# > 3 - Plus de 80 pour cent du sol est recouvert de résidus ou de cultures de couverture. Les cultures sont alternées régulièrement ou intercalées (ou le pâturage en rotation est systématique). La perturbation du sol est minimisée.
# > 4 - Tout le sol est couvert de résidus ou de cultures de couverture. Les cultures sont tournées régulièrement et les cultures intercalaires sont courantes (ou le pâturage en rotation est systématique). Peu ou pas de perturbation du sol.

jointuresSolPlantes <- left_join(
  rga23_prodVegetales |> select(interview__key, SurfaceTotalProdAgri, totalSurfaceFourrages, ModesProduction__4),
  rga23_surfacesCultures |>
    count(interview__key, name = "nbCultures"),
  by = "interview__key"
) |>
  left_join(rga23_tape |> select(interview__key, PratiquesCulturales__1, PratiquesCulturales__2, PratiquesCulturales__5, PratiquesCulturales__3),
    by = "interview__key"
  ) |>
  left_join(nbCulturesArbresDeclarees |> select(interview__key, nbCulturesArbres),
    by = "interview__key"
  ) |>
  left_join(
    # Pour traiter le cas où la seule culture est "Divers fruitiers"
    rga23_surfacesCultures |>
      filter(culture_id == 331) |>
      mutate(DiversFruitiers = 1) |>
      select(interview__key, DiversFruitiers),
    by = "interview__key"
  )

score_2_SolPlantes <- jointuresSolPlantes |>
  left_join(rga23_general |> select(interview__key, RaisonsRecensement__1)) |>
  mutate(partPaturage = totalSurfaceFourrages / SurfaceTotalProdAgri * 100) |>
  mutate(score = case_when(
    # Pas de cultures -> non concerné
    RaisonsRecensement__1 == 0 ~ 99,
    # Aucune des pratiques (1,2,5) et plusieurs cultures (ou jardin océanien ou uniquement divers fruitiers) OU Au moins une pratique et 0 ou 1 culture basse
    (PratiquesCulturales__1 == 0 & PratiquesCulturales__2 == 0 & PratiquesCulturales__5 == 0 & (nbCultures > 1 | ModesProduction__4 == 1 | (nbCultures == 1 & DiversFruitiers == 1))) |
      (PratiquesCulturales__1 + PratiquesCulturales__2 + PratiquesCulturales__5 >= 1 & (nbCultures - (replace_na(nbCulturesArbres, 0)) <= 1)) ~ 1,
    # Une seule des 3 pratiques + plusieurs cultures basses (ou jardin océanien)
    (PratiquesCulturales__1 + PratiquesCulturales__2 + PratiquesCulturales__5 == 1) & (nbCultures > (replace_na(nbCulturesArbres, 0) + 1) | ModesProduction__4 == 1) ~ 2,
    # Les 3 pratiques + plusieurs cultures basses (ou jardin océanien) . Pas de labour
    (PratiquesCulturales__1 + PratiquesCulturales__2 + PratiquesCulturales__5 == 3) & (nbCultures > (replace_na(nbCulturesArbres, 0) + 1) | ModesProduction__4 == 1) & PratiquesCulturales__3 == 0 ~ 4,
    # Au moins 2 des 3 pratiques + plusieurs cultures basses (ou jardin océanien)
    (PratiquesCulturales__1 + PratiquesCulturales__2 + PratiquesCulturales__5 >= 2) & (nbCultures > (replace_na(nbCulturesArbres, 0) + 1) | ModesProduction__4 == 1) ~ 3,
    # Monoculture (Hors cas où la seule culture est "Divers Fruitiers") - pas de jardin océanien
    (nbCultures == 1 & is.na(DiversFruitiers)) & (ModesProduction__4 == 0 | is.na(ModesProduction__4)) ~ 0,
    TRUE ~ 55
  ))

score_2_SolPlantes |>
  group_by(score) |>
  count()

# 2.3 INTEGRATION AVEC LES ARBRES (AGROFORESTERIE, SILVOPASTORALISME, AGROSILVOPASTORALISME)
# Considérez aussi les zones forestières communales.
# > 0 - Pas d’intégration: les arbres (et autres plantes vivaces) n’ont pas de rôle pour l’homme ou dans la production végétale ou animale.
# > 1 - Faible intégration: un petit nombre d’arbres (et autres plantes vivaces) ne fournissent qu’un seul produit (par exemple fruits, bois, fourrage, substances médicinales ou biopesticides…) ou qu’un seul service pour les cultures et/ou les animaux (par exemple de l’ombre pour les animaux, une fertilité accrue du sol, une rétention d’eau, une barrière à l’érosion du sol.
# > 2 - Intégration moyenne: un nombre important d’arbres (et d’autres plantes vivaces) fournissent au moins un produit ou un service.
# > 3 - Intégration élevée: un nombre important d’arbres (et autres plantes vivaces) fournissent plusieurs produits et services.
# > 4 - Intégration complète: de nombreux arbres (et autres vivaces) fournissent plusieurs produits et services.

# PsceArbresHorsRente__1 == 1 ~ "Présents en bord de parcelle",
# PsceArbresHorsRente__2 == 1 ~ "Présents dans la parcelle",
# PsceArbresHorsRente__3 == 1 ~ "Absents",

# RaisonsArbresHorsR__1
# Biodiversité................................1
# Biomasse (bois-énergie, bois d’oeuvre, …)...2
# Ombrage.....................................3
# Alimentation animale........................4
# Fertilisation...............................5
# Sans raison particulière....................6

score_3_IntegrationArbres <- left_join(rga23_tape,
  nbCulturesArbresDeclarees,
  by = "interview__key"
) |>
  left_join(rga23_prodVegetales |> select(interview__key, PartComFruit__13), by = "interview__key") |>
  mutate(score = case_when(
    ## Pas d'arbres, ni cultivés, ni hors rente
    is.na(nbCulturesArbres) & PsceArbresHorsRente__3 == 1 ~ 0,
    ## Uniquement des arbres hors rente en bord de parcelle pour de la biomasse ou sans raison
    is.na(nbCulturesArbres) & PsceArbresHorsRente__1 == 1 & PsceArbresHorsRente__2 == 0 &
      (RaisonsArbresHorsR__1 + RaisonsArbresHorsR__3 + RaisonsArbresHorsR__4 + RaisonsArbresHorsR__5 == 0) ~ 1,
    # Cultures d'arbre mais pas d'arbre hors rente -> 2
    nbCulturesArbres > 0 & PsceArbresHorsRente__3 == 1 ~ 2,
    # Pas de cultures d'arbres mais arbres hors rente présents dans la parcelle et/ou en bord de parcelle mais avec un service hors biomasse
    is.na(nbCulturesArbres) & (PsceArbresHorsRente__2 == 1 | (PsceArbresHorsRente__1 == 1 & (RaisonsArbresHorsR__1 + RaisonsArbresHorsR__3 + RaisonsArbresHorsR__4 + RaisonsArbresHorsR__5 > 0))) ~ 2,
    # Cultures d'arbre + arbres hors rente avec 1 ou 2 services
    nbCulturesArbres > 0 & PsceArbresHorsRente__3 == 0 &
      (RaisonsArbresHorsR__1 + RaisonsArbresHorsR__2 + RaisonsArbresHorsR__3 + RaisonsArbresHorsR__4 + RaisonsArbresHorsR__5 + ifelse((PartComFruit__13 == 0 | is.na(PartComFruit__13)), 1, 0) <= 2) ~ 3,
    # Cultures d'arbre + arbres hors rente avec au moins 3 services
    nbCulturesArbres > 0 & PsceArbresHorsRente__3 == 0 &
      (RaisonsArbresHorsR__1 + RaisonsArbresHorsR__2 + RaisonsArbresHorsR__3 + RaisonsArbresHorsR__4 + RaisonsArbresHorsR__5 + ifelse((PartComFruit__13 == 0 | is.na(PartComFruit__13)), 1, 0) >= 3) ~ 4,
    TRUE ~ 55
  ))

score_3_IntegrationArbres |>
  group_by(score) |>
  count()


# CONNECTIVITÉ ENTRE LES ÉLÉMENTS DE L’AGROÉCOSYSTÈME ET LE PAYSAGE
# Considérez les zones environnantes, les environnements semi-naturels et les zones potentielles de compensation écologique.
# > 0 - Pas de connectivité: grande uniformité à l’intérieur et à l’extérieur de l’agroécosystème, pas d’environnements semi-naturels, pas de zones de compensation écologique.
# > 1 - Faible connectivité: quelques éléments isolés peuvent être trouvés dans l’agroécosystème, tels que des arbres, des arbustes, des clôtures naturelles, un étang ou une petite zone de compensation écologique.
# > 2 - Connectivité moyenne: plusieurs éléments sont adjacents aux cultures et/ou pâturages ou à une grande zone de compensation écologique.
# > 3 - Connectivité importante: plusieurs éléments peuvent être trouvés entre des parcelles de cultures et/ou des pâturages ou plusieurs zones de compensation écologique (arbres, arbustes, végétation naturelle, pâturages, haies, canaux, etc.).
# > 4 - Connectivité élevée: l’agroécosystème présente une mosaïque et un paysage diversifié, de nombreux éléments tels que des arbres, des arbustes, des clôtures ou des étangs peuvent être trouvés entre chaque parcelle de terrain ou pâturage, ou plusieurs zones de compensation écologique.
#
