# 7. CO-CRÉATION & PARTAGE DE CONNAISSANCES

# EvenComLocale : Y-a-t-il des évenements/animations/rencontres de la communauté locale agricole sur votre île (marché, réunion, ...) ?
ilesAvecPresenceAveree <- left_join(rga23_tape,
  rga23_exploitations |> select(interview__key, IleExploitation),
  by = "interview__key"
) |>
  filter(EvenComLocale == 1) |>
  distinct(IleExploitation) |>
  mutate(presenceAveree = 1)

# PLATEFORMES POUR LA CRÉATION ET LE TRANSFERT HORIZONTAL DE CONNAISSANCES ET DE BONNES PRATIQUES
# Les plateformes peuvent être des organisations formelles ou informelles, des écoles pratiques d’agriculteurs, des réunions régulières, des formations, etc.
# > 0 - Aucune plateforme de co-création et partage de connaissances n’est disponible pour les producteurs.
# > 1 - Au moins une plateforme de co-création et partage de connaissances existe mais ne fonctionne pas bien et/ou n’est pas utilisée dans la pratique.
# > 2 - Au moins une plateforme de co-création et partage de connaissances existe et fonctionne mais n’est pas utilisée pour partager spécifiquement les connaissances sur l’agroécologie.
# > 3 – Une ou plusieurs plateformes de co-création et partage de connaissances existent, fonctionnent et sont utilisées pour partager les connaissances sur l’agroécologie, comprenant les femmes.
# > 4 – Plusieurs plates-formes bien établies et fonctionnelles pour la co-création et le partage de connaissances sont disponibles et répandues au sein de la communauté, comprenant les femmes.

# EnvProLocalExpl
# Isolé.................................................................................................1
# Adhérent d’une structure agricole (association, coopérative)..........................................2
# Engagé dans des instances décisionnaires (CAPL, commissions agricoles des communes, syndicats, ...)...3
# Inscrit sur des sites Facebook ou autres traitant de l’agriculture....................................4
# Visité par des techniciens agricoles pour du conseil..................................................5

score_1_Plateformes <- left_join(rga23_tape,
  rga23_exploitations |> select(interview__key, IleExploitation),
  by = "interview__key"
) |>
  left_join(ilesAvecPresenceAveree, by = "IleExploitation") |>
  mutate(score = case_when(
    # ISOLE + pas de plateforme OU Ne sait pas s'il existe des plateformes et AUCUN autre exploitant de l'île a répondu OUI
    EnvProLocalExpl__1 == 1 & (EvenComLocale == 2 | EvenComLocale == 3) & is.na(presenceAveree)  ~ 0,
    # ISOLE + Ne sait pas s'il existe des plateformes mais un autre exploitant de l'île a répondu OUI
    EnvProLocalExpl__1 == 1 & (EvenComLocale == 2 | EvenComLocale == 3) & presenceAveree == 1 ~ 1,
    # Adhérent d’une structure agricole (association, coopérative) OU  facebook
    EnvProLocalExpl__2 == 1 | EnvProLocalExpl__4 == 1 ~ 2,
    TRUE ~ 55
  ))

score_1_Plateformes |>
  group_by(score) |>
  count()

score_1_Plateformes |>
  filter(score == 55) |>
  group_by(EnvProLocalExpl__1, EvenComLocale) |>
  count()

# ACCÈS AUX CONNAISSANCES AGROÉCOLOGIQUES ET INTÉRÊT DES PRODUCTEURS À L’AGROÈCOLOGIE
# Les connaissances et pratiques agroécologiques peuvent également être appelées d’une autre manière, et les producteurs peuvent les connaître et les appliquer sans connaître le mot «agroécologie». Concentrez-vous sur les pratiques et les connaissances réelles pour l’évaluation, et non sur les connaissances formelles de «l’agroécologie» en tant que science.
# > 0 - Manque d’accès aux connaissances agroécologiques: les producteurs ignorent les principes de l’agroécologie.
# > 1 - Les principes de l’agroécologie sont pour la plupart inconnus aux producteurs et/ou il y a peu de confiance en eux.
# > 2 - Certains principes agroécologiques sont connus aux producteurs et il existe un intérêt à diffuser l’innovation, à faciliter le partage des connaissances au sein des communautés et à impliquer les jeunes générations.
# > 3 – L’agroécologie est bien connue et les producteurs sont prêts à mettre en œuvre ses innovations, à faciliter le partage des connaissances au sein des communautés et à impliquer les jeunes générations, y compris les femmes.
# > 4 - Accès généralisé aux connaissances agroécologiques des hommes et des femmes: les producteurs sont bien conscients des principes de l’agroécologie et désireux de les appliquer, en facilitant le partage des connaissances au sein des communautés et en impliquant les jeunes générations.

score_2_AccesConnaissances <- left_join(rga23_tape,
  rga23_exploitations |> select(interview__key, TypePhytosanit__1, TypePhytosanit__2, TypeEngrais__1, TypeEngrais__2, TypeEngrais__3, AgriBio_DAG),
  by = "interview__key"
) |>
  mutate(score = case_when(
    # Bio_DAG -> 4
    AgriBio_DAG == 1 ~ 4,
    # Que chimique + ne connait pas les pratiques (ConnaissPratiques) -> 0
    ConnaissPratiques == 2 ~ 0,
    # Que chimique + connait les pratiques -> 1
    ConnaissPratiques == 1 ~ 1,
    # Mixte chimique et bio -> 2
    (replace_na(TypePhytosanit__1, 0) + replace_na(TypeEngrais__1, 0) >= 1) &
      (replace_na(TypePhytosanit__2, 0) + replace_na(TypeEngrais__2, 0) + replace_na(TypeEngrais__3, 0) >= 1) ~ 2,
    # Pas de chimique (produits bio ou rien) -> 3
    replace_na(TypePhytosanit__1, 0) == 0 & replace_na(TypeEngrais__1, 0) == 0 ~ 3,
    TRUE ~ 55
  ))

score_2_AccesConnaissances |>
  group_by(score) |>
  count()

restent <- score_2_AccesConnaissances |> filter(score == 55) |> group_by(TypePhytosanit__1, TypeEngrais__1)  |>count()


# PARTICIPATION DES PRODUCTEURS AUX RÉSEAUX ET AUX ORGANISATIONS DE BASE
# Avec perspective de genre.
# > 0 - Les producteurs sont isolés, n’ont pratiquement aucune relation avec leur communauté locale et ne participent pas aux réunions et aux organisations locales.
# > 1 - Les producteurs entretiennent des relations sporadiques avec leur communauté locale et participent rarement aux réunions et aux organisations locales.
# > 2 - Les producteurs entretiennent des relations régulières avec leur communauté locale et participent parfois aux événements de leurs organisations locales mais pas autant pour les femmes.
# > 3 - Les producteurs sont bien interconnectés avec leur communauté locale et participent souvent aux événements de leurs organisations locales, y compris les femmes.
# > 4 - Les producteurs (avec une participation égale des hommes et des femmes) sont fortement interconnectés et solidaires et montrent un engagement et une participation très élevés à tous les événements de leur section locale.

score_3_Participation <- left_join(rga23_tape,
  rga23_exploitations |> select(interview__key, IleExploitation),
  by = "interview__key"
) |>
  left_join(ilesAvecPresenceAveree, by = "IleExploitation") |>
  mutate(score = case_when(
    # Existence de plateformes + il y participe jamais OU pas de plateforme sur l'île et il se sent isolé
    (presenceAveree == 1 & (is.na(FreqEvenComLocale) | FreqEvenComLocale == 1)) | (is.na(presenceAveree) & EnvProLocalExpl__1 == 1) ~ 0,
    # Existence de plateformes + il y participe rarement
    presenceAveree == 1 & FreqEvenComLocale == 2 ~ 1,
    # Existence de plateformes + il y participe parfois
    presenceAveree == 1 & FreqEvenComLocale == 3 ~ 2,
    # Existence de plateformes + il y participe souvent
    presenceAveree == 1 & FreqEvenComLocale == 4 ~ 3,
    # Existence de plateformes + il y participe toujours
    presenceAveree == 1 & FreqEvenComLocale == 5 ~ 4,
    TRUE ~ 55
  ))

score_3_Participation |>
  group_by(score) |>
  count()

score_3_Participation |>
  filter(score == 55) |>
  group_by(EnvProLocalExpl__2, EnvProLocalExpl__3, EnvProLocalExpl__4, EnvProLocalExpl__5) |>
  count()

