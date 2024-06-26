## Champ : 4080 exploitations au sens du RGA 2023

## Restriction au champ et récupération des variables Ile et Commune
rga23_champ_Ile_Commune <- left_join(readCSV("rga23_general.csv") |> filter(indicRGA23 == 1),
  readCSV("rga23_exploitations.csv") |> select(interview__key, eligibilite, IleExploitation, CommuneExploitation),
  by = "interview__key"
) |>
  left_join(readCSV("rga23_mainOeuvre.csv") |> select(interview__key, Ile, Commune),
    by = "interview__key"
  ) |>
  left_join(
    readCSV("rga23_coprahculteurs.csv") |> select(interview__key, eligibiliteCoprah),
    by = "interview__key"
  ) |>
  mutate(
    Cultivateurs = ifelse(indicRGA23 == 1 & RaisonsRecensement__1 == 1 & eligibilite == 1, 1, 0),
    Eleveurs = ifelse(indicRGA23 == 1 & RaisonsRecensement__2 == 1 & eligibilite == 1, 1, 0),
    ProducteursCoprah = ifelse(indicRGA23 == 1 & RaisonsRecensement__3 == 1 & eligibiliteCoprah == 1, 1, 0),
    Ile = case_when(
      !is.na(IleExploitation) ~ IleExploitation,
      TRUE ~ Ile
    ),
    Commune = case_when(
      !is.na(CommuneExploitation) ~ CommuneExploitation,
      TRUE ~ Commune
    )
  ) |>
  select(-IleExploitation, -CommuneExploitation, -starts_with("indic"), -starts_with("eligibilite"), -statut_collecte, -starts_with("RaisonsRecensement"), -PointsCAPL, -ActiviteEnquete)

## TCD 1 : nombres d'exploitants (cultures et/ou élevage et/ou coprah)
TCD1 <- rga23_champ_Ile_Commune
writeCSV(TCD1)

## TCD 2 : caractéristiques des chefs d'exploitations (hommes/femmes, âge)
TCD2 <- left_join(rga23_champ_Ile_Commune,
  readCSV("rga23_mainOeuvre.csv") |> mutate(
    age = 2023 - as.numeric(substring(DateNaissChefExpl, 7, 10)),
    homme = case_when(SexeChefExpl == 1 ~ 0, SexeChefExpl == 2 ~ 1),
    femme = case_when(SexeChefExpl == 1 ~ 1, SexeChefExpl == 2 ~ 0)
  ) |>
    select(interview__key, homme, femme, age),
  by = "interview__key"
)
writeCSV(TCD2)

## TCD 3 : SAU totale déclarée
TCD3 <- inner_join(rga23_champ_Ile_Commune,
  readCSV("rga23_prodVegetales.csv") |> select(interview__key, SurfaceTotalProdAgri),
  by = "interview__key"
)
writeCSV(TCD3)

## TCD 4 : détail des surfaces végétales dont jachères par type
recalculSurfacesType <- inner_join(
  rga23_champ_Ile_Commune,
  readCSV("rga23_surfacesCultures.csv"),
  by = "interview__key"
) |>
  # Exclusion des pâturages et des cocoteraies
  filter(culture_id != 701 & culture_id != 702 & culture_id != 705 & culture_id != 307 & culture_id != 308 & culture_id != 309) |>
  group_by(interview__key, TypeCulture) |>
  summarize(SurfacesTotalesClassiques = sum(ifelse(is.na(SurfaceCult), 0, SurfaceCult))) |>
  mutate(TypeCulture = case_when(
    (TypeCulture == 10) ~ "10 - Cultures maraîchères",
    (TypeCulture == 20) ~ "20 - Cultures vivrières",
    (TypeCulture == 30) ~ "30 - Cultures fruitières (hors cocoteraies)",
    (TypeCulture == 40) ~ "40 - Feuillages et cultures florales (hors pépinières)",
    (TypeCulture == 50) ~ "50 - Plantes aromatiques, stimulantes et médicinales",
    (TypeCulture == 60) ~ "60 - Pépinières (plantes vendues en pot)",
    (TypeCulture == 70) ~ "70 - Cultures fourragères (hors pâturages)",
    (TypeCulture == 80) ~ "80 - Jachères",
    TRUE ~ as.character(TypeCulture)
  )) |>
  spread(key = TypeCulture, value = SurfacesTotalesClassiques, fill = 0)

TCD4 <- inner_join(rga23_champ_Ile_Commune,
  readCSV("rga23_prodVegetales.csv") |> select(interview__key, SurfaceJardins),
  by = "interview__key"
) |>
  left_join(recalculSurfacesType,
    by = "interview__key"
  ) |>
  mutate(across(everything(), ~ ifelse(is.na(.), 0, .)))
writeCSV(TCD4)

# TCD 5 : détail des surfaces végétales classiques (y compris des pâturages)
TCD5 <- inner_join(
  rga23_champ_Ile_Commune,
  readCSV("rga23_surfacesCultures.csv"),
  by = "interview__key"
) |>
  # Exclusion uniquement des cocoteraies
  filter(culture_id != 307 & culture_id != 308 & culture_id != 309) |>
  mutate(TypeCulture = case_when(
    (TypeCulture == 10) ~ "10 - Cultures maraîchères",
    (TypeCulture == 20) ~ "20 - Cultures vivrières",
    (TypeCulture == 30) ~ "30 - Cultures fruitières (hors cocoteraies)",
    (TypeCulture == 40) ~ "40 - Feuillages et cultures florales (hors pépinières)",
    (TypeCulture == 50) ~ "50 - Plantes aromatiques, stimulantes et médicinales",
    (TypeCulture == 60) ~ "60 - Pépinières (plantes vendues en pot)",
    (TypeCulture == 70) ~ "70 - Cultures fourragères (Y COMPRIS pâturages)",
    (TypeCulture == 80) ~ "80 - Jachères",
    TRUE ~ as.character(TypeCulture)
  )) |>
  left_join(readInputCSV("cultures.csv"),
    by = "culture_id"
  ) |>
  mutate(culture = paste0(libelleCulture, " (", culture_id, ")")) |>
  select(interview__key, Archipel_1, Ile, Commune, TypeCulture, culture_id, culture, SurfaceCult)
writeCSV(TCD5)

# TCD 6 : nombre d'éleveurs et effectifs par espèces
TCD6 <- inner_join(
  rga23_champ_Ile_Commune,
  readCSV("rga23_prodAnimales.csv"),
  by = "interview__key"
) |>
  mutate(
    EleveurBovins = PresenceAnimaux__1,
    EleveurOvins = PresenceAnimaux__2,
    EleveurPorcins = PresenceAnimaux__3,
    EleveurVolailles = PresenceAnimaux__4,
    EleveurPoulesPondeuses = ifelse(PresenceAnimaux__4 == 1 & (TypeVolailles__1 == 1 | TypeVolailles__3 == 1 | TypeVolailles__4 == 1), 1, 0),
    EleveurEquides = PresenceAnimaux__5,
    EleveurLapins = PresenceAnimaux__6,
    Apiculteurs = PresenceAnimaux__7,
    EleveurCaprins = PresenceAnimaux__8,
    NombreBovins = replace_na(nbTotalBovins, 0),
    NombreOvins = replace_na(nbTotalOvins, 0),
    NombrePorcins = replace_na(nbTotalPorcs, 0),
    NombreVolailles = rowSums(across(
      c("NbAutresVolailles", "NbDindesDindons", "NbOies", "NbCanards", "NbCailles", "NbPintades", "NbPouletsChairCoqs", "NbPoulettes", "NbPoussins", "NombrePoules0", "NombrePoules1", "NombrePoules3"),
      ~ (coalesce(., 0))
    )),
    NombrePoulesPondeuses = rowSums(across(
      c("NombrePoules0", "NombrePoules1", "NombrePoules3"),
      ~ (coalesce(., 0))
    )),
    NombreEquides = replace_na(nbTotalEquides, 0),
    NombreLapins = rowSums(across(
      c("NbLapereaux", "NbLapinesFutures", "NbLapinesMeres", "NbLapinsReprod", "NbLapinsSevresEngrais"),
      ~ (coalesce(., 0))
    )),
    NombreRuchesPourProduire = replace_na(NbRuchesPourProduire, 0),
    NombreCaprins = replace_na(nbTotalCaprins, 0)
  ) |>
  select(
    interview__key,
    Archipel_1,
    Ile,
    Commune,
    EleveurBovins,
    EleveurOvins,
    EleveurPorcins,
    EleveurVolailles,
    EleveurPoulesPondeuses,
    EleveurEquides,
    EleveurLapins,
    Apiculteurs,
    EleveurCaprins,
    NombreBovins,
    NombreOvins,
    NombrePorcins,
    NombreVolailles,
    NombrePoulesPondeuses,
    NombreEquides,
    NombreLapins,
    NombreRuchesPourProduire,
    NombreCaprins
  )
writeCSV(TCD6)

# TCD 7 : Zoom sur les éleveurs Bovins
TCD7 <- inner_join(
  rga23_champ_Ile_Commune,
  readCSV("rga23_prodAnimales.csv"),
  by = "interview__key"
) |>
  filter(PresenceAnimaux__1 == 1) |>
  select(
    interview__key,
    Archipel_1,
    Ile,
    Commune,
    nbTotalBovins,
    NbAutresBovinsLait,
    NbAutresBovinsViande,
    NbBovinsAttache,
    NbBovinsLiberte,
    NbBovinsPaturage,
    NbGenissesLait,
    NbGenissesViande,
    NbJeunesEngrLait,
    NbJeunesEngrViande,
    nbTotalBovins,
    nbTotalBovinsLait,
    nbTotalBovinsViande,
    NbTaureauxLait,
    NbTaureauxViande,
    NbVachesLait,
    NbVachesViande,
    NbVeauxLait,
    NbVeauxViande
  ) |>
  mutate(across(where(is.numeric), ~ coalesce(., 0)))
writeCSV(TCD7)

# TCD 8 : Production d'oeufs
TCD8 <- inner_join(
  rga23_champ_Ile_Commune,
  readCSV("rga23_prodAnimales.csv"),
  by = "interview__key"
) |>
  filter(PresenceAnimaux__4 == 1 & (TypeVolailles__1 == 1 | TypeVolailles__3 == 1 | TypeVolailles__4 == 1)) |>
  mutate(
    EleveursPoulesEnCage = ifelse(replace_na(NombrePoules3, 0) > 0, 1, 0),
    PoulesEnCage = replace_na(NombrePoules3, 0),
    NbOeufsPoulesEnCage = replace_na(ProductionPoules3, 0),
    EleveursAutresPoules = ifelse(replace_na(NombrePoules0, 0) > 0 | replace_na(NombrePoules1, 0) > 0, 1, 0),
    AutresPoules = replace_na(NombrePoules0, 0) + replace_na(NombrePoules1, 0),
    NbOeufsAutresPoules = replace_na(ProductionPoules0, 0) + replace_na(ProductionPoules1, 0)
  ) |>
  select(
    interview__key,
    Archipel_1,
    Ile,
    Commune,
    EleveursPoulesEnCage,
    PoulesEnCage,
    NbOeufsPoulesEnCage,
    EleveursAutresPoules,
    AutresPoules,
    NbOeufsAutresPoules
  )
writeCSV(TCD8)

# TCD 9 : Production de miel
TCD9 <- inner_join(
  rga23_champ_Ile_Commune,
  readCSV("rga23_prodAnimales.csv"),
  by = "interview__key"
) |>
  filter(PresenceAnimaux__7 == 1) |>
  select(
    interview__key,
    Archipel_1,
    Ile,
    Commune,
    NbRuchettes,
    NbRuchesPourProduire,
    NbRuchesRecoltees,
    ProductionRuches,
    ProductionExporteeRuches
  ) |>
  mutate(across(where(is.numeric), ~ coalesce(., 0)))
writeCSV(TCD9)

# TCD 10 : Zoom sur les éleveurs porcins
TCD10 <- inner_join(
  rga23_champ_Ile_Commune,
  readCSV("rga23_prodAnimales.csv"),
  by = "interview__key"
) |>
  filter(PresenceAnimaux__3 == 1) |>
  select(
    interview__key,
    Archipel_1,
    Ile,
    Commune,
    nbTotalPorcs,
    NbTruiesMaternite,
    NbPorceletsNonSevres,
    NbTruiesGestVides,
    NbCochettes,
    NbPorceletsPostSevrage,
    NbPorcsEngraissement,
    NbVerrats,
    NbAutresPorcs,
    AccesBatimentPorcins,
    starts_with("TypeBatimentPorcins"),
    AccesParcoursPorcins
  ) |>
  mutate(across(where(is.numeric), ~ coalesce(., 0)),
    AccesParcoursTous = ifelse(AccesParcoursPorcins == 1, 1, 0),
    AccesParcoursUneParite = ifelse(AccesParcoursPorcins == 3, 1, 0),
    PasAccesParcous = ifelse(AccesParcoursPorcins == 2, 1, 0),
    PasAccesBatiment = ifelse(AccesBatimentPorcins == 2, 1, 0)
  ) |>
  select(-AccesParcoursPorcins, -AccesBatimentPorcins)
writeCSV(TCD10)

# TCD 11 : Effectifs et ETP
source("TCD/ExportsTCD_IleCommune_ETP.R")

# TCD 12 : Matériel de traction et de transport

# Bateau à usage agricole.......................1/1
# Bétaillère....................................2/2
# Bulldozer (case)..............................3/3
# Mini-pelle hydraulique (Pel-Job)..............4/4
# Motoculteur...................................5/5
# Quad..........................................6/6
# Tracteur de moins de 50 CV....................7/7
# Tracteur de plus de 50 CV et moins de 90 CV...8/8
# Tracteur de plus de 90 CV.....................9/9
# Tractopelle (drague)..........................10/10
# Véhicule de livraison.........................11/11
# Autre.........................................12/12
# Aucune de ces propositions....................999/999

TCD12 <- inner_join(
  rga23_champ_Ile_Commune,
  readCSV("rga23_exploitations.csv") |> filter(eligibilite == 1) |> select(interview__key, starts_with("MaterielTransport")) |> select(-MaterielTransport__13),
  by = "interview__key"
) |> rename(
  `Bateau à usage agricole` = MaterielTransport__1,
  `Bétaillère` = MaterielTransport__2,
  `Bulldozer (case)` = MaterielTransport__3,
  `Mini-pelle hydraulique (Pel-Job)` = MaterielTransport__4,
  `Motoculteur` = MaterielTransport__5,
  `Quad` = MaterielTransport__6,
  `Tracteur de moins de 50 CV` = MaterielTransport__7,
  `Tracteur de plus de 50 CV et moins de 90 CV` = MaterielTransport__8,
  `Tracteur de plus de 90 CV` = MaterielTransport__9,
  `Tractopelle (drague)` = MaterielTransport__10,
  `Véhicule de livraison` = MaterielTransport__11,
  `Autre` = MaterielTransport__12,
  `Aucune de ces propositions` = MaterielTransport__999
)
writeCSV(TCD12)

# TCD 13 : Matériel de travail et entretien du sol

# Charrue........................1
# Matériel à dents...............2
# Matériel à disques.............3
# Matériel à lames (rotavator)...4
# Petit matériel (manuel)........5
# Autre..........................6
# Aucune de ces propositions.....999

TCD13 <- inner_join(
  rga23_champ_Ile_Commune |> filter(Cultivateurs == 1),
  readCSV("rga23_exploitations.csv") |> select(interview__key, starts_with("MaterielTravailSol")),
  by = "interview__key"
) |> rename(
  `Charrue` = MaterielTravailSol__1,
  `Matériel à dents` = MaterielTravailSol__2,
  `Matériel à disques` = MaterielTravailSol__3,
  `Matériel à lames (rotavator)` = MaterielTravailSol__4,
  `Petit matériel (manuel)` = MaterielTravailSol__5,
  `Autre` = MaterielTravailSol__6,
  `Aucune de ces propositions` = MaterielTravailSol__999
)
writeCSV(TCD13)

# TCD 14 : Materiel pour les semis, plantations et entretiens des cultures

# Semoir.....................................1
# Planteuse..................................2
# Epandeur d'engrais.........................3
# Pulvérisateur tracté.......................4
# Pulvérisateur thermique/électrique porté...5
# Pulvérisateur manuel (pompe)...............6
# Aucune de ces propositions.................999

TCD14 <- inner_join(
  rga23_champ_Ile_Commune |> filter(Cultivateurs == 1),
  readCSV("rga23_exploitations.csv") |> select(interview__key, starts_with("MatSemisPlant_")),
  by = "interview__key"
) |> rename(
  `Semoir` = MatSemisPlant__1,
  `Planteuse` = MatSemisPlant__2,
  `Epandeur d'engrais` = MatSemisPlant__3,
  `Pulvérisateur tracté` = MatSemisPlant__4,
  `Pulvérisateur thermique/électrique porté` = MatSemisPlant__5,
  `Pulvérisateur manuel (pompe)` = MatSemisPlant__6,
  `Aucune de ces propositions` = MatSemisPlant__999
)
writeCSV(TCD14)

# TCD 15 : Materiel pour l'épandage et l'enfouissement des déjections animales

# Buse palette ou rampe d'épandage (cuve à lisier)....1
# Enfouisseur / injecteur à dents (en surface)........2
# Enfouisseur / injecteur à disques (en profondeur)...3
# Epandage manuel.....................................4
# Epandeur à fumier...................................5
# Pendillard avec sabots traîné.......................6
# Pendillard avec tuyaux traînés......................7
# Autre...............................................8

TCD15 <- inner_join(
  rga23_champ_Ile_Commune |> filter(Cultivateurs == 1),
  readCSV("rga23_exploitations.csv") |> select(interview__key, starts_with("MaterielEpandage")) |> filter(!is.na(MaterielEpandage__1)),
  by = "interview__key"
) |> rename(
  `Buse palette ou rampe d'épandage (cuve à lisier)` = MaterielEpandage__1,
  `Enfouisseur / injecteur à dents (en surface)` = MaterielEpandage__2,
  `Enfouisseur / injecteur à disques (en profondeur)` = MaterielEpandage__3,
  `Epandage manuel` = MaterielEpandage__4,
  `Epandeur à fumier` = MaterielEpandage__5,
  `Pendillard avec sabots traînés` = MaterielEpandage__6,
  `Pendillard avec tuyaux traînés` = MaterielEpandage__7,
  `Autre` = MaterielEpandage__8
)
writeCSV(TCD15)

# TCD 16 : Materiel de traitement de la récolte

# Calibreuse........................................1/1
# Laveuse de légumes................................2/2
# Eplucheuse........................................3/3
# Séchoir solaire...................................4/4
# Séchoir thermique.................................5/5
# Installation de stockage au froid.................6/6
# Installation de stockage à température ambiante...7/7
# Aucune de ces propositions........................999/999

TCD16 <- inner_join(
  rga23_champ_Ile_Commune |> filter(Cultivateurs == 1),
  readCSV("rga23_exploitations.csv") |> select(interview__key, starts_with("MaterielTraitRecolte")) |> filter(!is.na(MaterielTraitRecolte__1)),
  by = "interview__key"
) |> rename(
  `Calibreuse` = MaterielTraitRecolte__1,
  `Laveuse de légumes` = MaterielTraitRecolte__2,
  `Eplucheuse` = MaterielTraitRecolte__3,
  `Séchoir solaire` = MaterielTraitRecolte__4,
  `Séchoir thermique` = MaterielTraitRecolte__5,
  `Installation de stockage au froid` = MaterielTraitRecolte__6,
  `Installation de stockage à température ambiante` = MaterielTraitRecolte__7,
  `Aucune de ces propositions` = MaterielTraitRecolte__999
)
writeCSV(TCD16)

# TCD 17 : Utilisation de produits phytosanitaires

## Utilisez-vous des médicaments pour vos animaux ou au moins l'un des produits phytosanitaires suivants ? UtilisationPhytosanit
## De quel(s) type(s) sont les produits phytosanitaires que vous utilisez ? TypePhytosanit__
## Sur quelles espèces/cultures utilisez-vous ces médicaments ou produits phytosanitaires chimiques ? NbCultEspPhytoChim

TCD17 <- inner_join(
  rga23_champ_Ile_Commune,
  readCSV("rga23_exploitations.csv") |>
    filter(eligibilite == 1) |>
    mutate(
      `Toutes les Cultures et Especes` = ifelse(NbCultEspPhytoChim == 1, 1, 0),
      `Une Partie des Cultures et Especes` = ifelse(NbCultEspPhytoChim == 2, 1, 0),
      `Une Seule Culture Espece` = ifelse(NbCultEspPhytoChim == 3, 1, 0),
      UtilisationPhytosanit = case_when(
        UtilisationPhytosanit == 1 ~ 1,
        UtilisationPhytosanit == 2 ~ 0
      ),
      FormationPhytosanit = case_when(
        FormationPhytosanit == 1 ~ 1,
        FormationPhytosanit == 2 ~ 0
      )
    ) |>
    rename(
      `Produits Synthétiques (chimique)` = TypePhytosanit__1,
      `Produits Biologiques` = TypePhytosanit__2
    ) |>
    select(
      interview__key, UtilisationPhytosanit,
      `Produits Synthétiques (chimique)`, `Produits Biologiques`,
      `Toutes les Cultures et Especes`, `Une Partie des Cultures et Especes`, `Une Seule Culture Espece`,
      FormationPhytosanit
    ),
  by = "interview__key"
)
writeCSV(TCD17)

## TCD18 : Formation

TCD18 <- inner_join(
  rga23_champ_Ile_Commune,
  readCSV("rga23_mainOeuvre.csv") |> select(-Ile, -Commune),
  by = "interview__key"
) |>
  mutate(
    `FNA - 1 : Aucune` = ifelse(FormNAChefExpl == 1, 1, 0),
    `FNA - 2 : Primaire` = ifelse(FormNAChefExpl == 2, 1, 0),
    `FNA - 3 : Secondaire court` = ifelse(FormNAChefExpl == 3, 1, 0),
    `FNA - 4 : Secondaire long` = ifelse(FormNAChefExpl == 4, 1, 0),
    `FNA - 5 : Supérieure` = ifelse(FormNAChefExpl == 5, 1, 0),
    `FNA - Non réponse` = ifelse(is.na(FormNAChefExpl), 1, 0),
    `FA - 1 : Aucune / Formation sur le tas` = ifelse(FormAgriChefExpl == 1, 1, 0),
    `FA - 2 : MFR, CJA sans obtention de diplôme` = ifelse(FormAgriChefExpl == 2, 1, 0),
    `FA - 3 : Secondaire courte (CAPA, BEPA)` = ifelse(FormAgriChefExpl == 3, 1, 0),
    `FA - 4 : Secondaire longue (BTA, Bac agricole)` = ifelse(FormAgriChefExpl == 4, 1, 0),
    `FA - 5 : Supérieure courte (BTSA)` = ifelse(FormAgriChefExpl == 5, 1, 0),
    `FA - 6 : Supérieure longue (ingénieur)` = ifelse(FormAgriChefExpl == 6, 1, 0),
    `FA - Non réponse` = ifelse(is.na(FormAgriChefExpl), 1, 0),
    FormationContinue = case_when(
      FormationContinue == 1 ~ 1,
      FormationContinue == 2 ~ 0
    )
  ) |>
  select(
    interview__key,
    Archipel_1,
    Ile,
    Commune,
    starts_with("FNA - "),
    starts_with("FA - "),
    FormationContinue
  )
writeCSV(TCD18)

## TCD19 : Temps de travail Chef exploitation

TCD19 <- inner_join(
  rga23_champ_Ile_Commune,
  readCSV("rga23_mainOeuvre.csv") |> select(-Ile, -Commune),
  by = "interview__key"
) |>
  mutate(
    `1 : Moins de 1/2 temps` = ifelse(TpsTravailChefExpl == 1, 1, 0),
    `2 : 1/2 temps` = ifelse(TpsTravailChefExpl == 2, 1, 0),
    `3 : Entre 1/2 temps et temps complet` = ifelse(TpsTravailChefExpl == 3, 1, 0),
    `4 : Temps complet` = ifelse(TpsTravailChefExpl == 4, 1, 0),
    `Non réponse` = ifelse(is.na(TpsTravailChefExpl), 1, 0)
  ) |>
  select(
    interview__key,
    Archipel_1,
    Ile,
    Commune,
    `1 : Moins de 1/2 temps`,
    `2 : 1/2 temps`,
    `3 : Entre 1/2 temps et temps complet`,
    `4 : Temps complet`,
    `Non réponse`
  )
writeCSV(TCD19)

# TCD 20 : Surfaces végétales classiques (hors pâturages et hors cocoteraies) par mode de production
TCD20 <- inner_join(
  rga23_champ_Ile_Commune,
  readCSV("rga23_surfacesCultures.csv") |>
    filter(culture_id != 701 & culture_id != 702 & culture_id != 705 & culture_id != 307 & culture_id != 308 & culture_id != 309) |>
    left_join(readInputCSV("cultures.csv"),
      by = "culture_id"
    ) |>
    mutate(ModeProduction = case_when(
      grepl("Plein air ou abri bas", libelleCulture, ignore.case = TRUE) ~ "Plein air ou abri bas",
      grepl("Serre ou abri haut pleine terre", libelleCulture, ignore.case = TRUE) ~ "Serre ou abri haut pleine terre",
      grepl("Serre ou abri haut hors sol", libelleCulture, ignore.case = TRUE) ~ "Serre ou abri haut hors sol",
      grepl("Serre ou abri haut", libelleCulture, ignore.case = TRUE) ~ "Serre ou abri haut",
      grepl("Plein air hors sol", libelleCulture, ignore.case = TRUE) ~ "Plein air hors sol",
      grepl("Plein champ", libelleCulture, ignore.case = TRUE) ~ "Plein champ",
      grepl("Sous ombrière", libelleCulture, ignore.case = TRUE) ~ "Sous ombrière",
      grepl("Sous serre", libelleCulture, ignore.case = TRUE) ~ "Sous serre",
      grepl("ombrage naturel", libelleCulture, ignore.case = TRUE) ~ "Ombrage naturel",
      grepl("serre ou ombrière", libelleCulture, ignore.case = TRUE) ~ "Serre ou ombrière",
      grepl("Plein air", libelleCulture, ignore.case = TRUE) ~ "Plein air",
      TRUE ~ "Non précisé"
    )) |>
    group_by(interview__key, ModeProduction) |>
    summarize(SurfacesModeProduction = sum(ifelse(is.na(SurfaceCult), 0, SurfaceCult))) |>
    spread(key = ModeProduction, value = SurfacesModeProduction, fill = 0),
  by = "interview__key"
)
writeCSV(TCD20)

# TCD 21 : Utilisation des engrais

## UtilisationEngrais
## TypeEngrais
### Engrais (ou amendements) de synthèse............1
### Engrais (ou amendements) organiques.............2
### Engrais (ou amendements) minéraux biologiques...3

TCD21 <- inner_join(
  rga23_champ_Ile_Commune,
  readCSV("rga23_exploitations.csv") |>
    filter(eligibilite == 1 & !is.na(UtilisationEngrais)) |>
    mutate(
      UtilisationEngrais = case_when(
        UtilisationEngrais == 1 ~ 1,
        UtilisationEngrais == 2 ~ 0
      )
    ) |>
    rename(
      `Engrais (ou amendements) de synthèse` = TypeEngrais__1,
      `Engrais (ou amendements) organiques` = TypeEngrais__2,
      `Engrais (ou amendements) minéraux biologiques` = TypeEngrais__3
    ) |>
    select(
      interview__key, UtilisationEngrais, starts_with("Engrais (ou amendements)")
    ),
  by = "interview__key"
)
writeCSV(TCD21)

# TCD 22 : Engrais/amendements organiques

# Lisier..................1
# Fumier..................2
# Fientes fraiches........3
# Fientes séchées.........7
# Engrais de poisson......4
# Compost et co-compost...5
# Autres..................6

TCD22 <- inner_join(
  rga23_champ_Ile_Commune,
  readCSV("rga23_engraisOrganiques.csv") |>
    mutate(
      NomEngraisOrganique = case_when(
        engraisOrga_id == 1 ~ "Lisier",
        engraisOrga_id == 2 ~ "Fumier",
        engraisOrga_id == 3 ~ "Fientes fraiches",
        engraisOrga_id == 7 ~ "Fientes séchées",
        engraisOrga_id == 4 ~ "Engrais de poisson",
        engraisOrga_id == 5 ~ "Compost et co-compost",
        engraisOrga_id == 6 ~ "Autres",
        TRUE ~ NA_character_
      ),
      VenteEngraisOrga = case_when(
        VenteEngraisOrga == 1 ~ 1,
        VenteEngraisOrga == 2 ~ 0
      ),
      DonEngraisOrga = case_when(
        DonEngraisOrga == 1 ~ 1,
        DonEngraisOrga == 2 ~ 0
      ),
      EpandageEngraisOrga = case_when(
        EpandageEngraisOrga == 1 ~ 1,
        EpandageEngraisOrga == 2 ~ 0
      ),
      EnfouissEngraisOrga = case_when(
        EnfouissEngraisOrga == 1 ~ 1,
        EnfouissEngraisOrga == 2 ~ 0
      )
    ) |>
    rename(
      `En provenance de l'exploitation` = OrigineEngraisOrga__1,
      `En provenance d'une autre exploitation ou d'un producteur de l'île` = OrigineEngraisOrga__2,
      `Importé depuis la Polynésie` = OrigineEngraisOrga__3,
      `Importé hors Polynésie` = OrigineEngraisOrga__4
    ),
  by = "interview__key"
)
writeCSV(TCD22)

# TCD 23 : Transformation de produits agricoles

# Transformation d’oléagineux (huiles de consommation ou pour combustible…).......................1/1
# Production d’huiles essentielles et hydrolats...................................................2/2
# Epluchage, découpe, conditionnement, 4ème gamme, ...............................................3/3
# Transformation de légumes (soupe, conserves…)...................................................4/4
# Transformation de racines ou tubercules (farine de manioc, kwak...).............................5/5
# Production issue de canne à sucre (jus de canne, rhum...).......................................6/6
# Transformation de vanille.......................................................................7/7
# Transformation de produits du cocotier (coprah, niau, lait de coco, …)..........................8/8
# Transformation de fruits (confitures, sirops, liqueurs, jus de fruits…).........................9/9
# Transformation de lait (beurre, yaourts, fromages, crème...)....................................10/10
# Abattage à la ferme.............................................................................11/11
# Transformation de viandes (pâtés, salaisons, conserves…)........................................12/12
# Découpe de viandes, caissettes….................................................................13/13
# Production de produits à base de miel...........................................................14/14
# Transformation d’autres produits agricoles (hors aliments pour les animaux de l’exploitation)...15/15
# Préparation de plats cuisinés...................................................................16/16
# Aucune transformation réalisée sur l'exploitation...............................................17/17

TCD23 <- inner_join(
  rga23_champ_Ile_Commune,
  readCSV("rga23_mainOeuvre.csv") |>
    select(interview__key, starts_with("TransformationPA")) |>
    mutate(`Au moins une transformation` = case_when(
      TransformationPA__17 == 0 ~ 1,
      TRUE ~ 0
    )) |>
    rename(
      "Transformation d’oléagineux (huiles de consommation ou pour combustible…)" = TransformationPA__1,
      "Production d’huiles essentielles et hydrolats" = TransformationPA__2,
      "Epluchage, découpe, conditionnement, 4ème gamme" = TransformationPA__3,
      "Transformation de légumes (soupe, conserves…)" = TransformationPA__4,
      "Transformation de racines ou tubercules (farine de manioc, kwak...)" = TransformationPA__5,
      "Production issue de canne à sucre (jus de canne, rhum...)" = TransformationPA__6,
      "Transformation de vanille" = TransformationPA__7,
      "Transformation de produits du cocotier (coprah, niau, lait de coco, …)" = TransformationPA__8,
      "Transformation de fruits (confitures, sirops, liqueurs, jus de fruits…)" = TransformationPA__9,
      "Transformation de lait (beurre, yaourts, fromages, crème...)" = TransformationPA__10,
      "Abattage à la ferme" = TransformationPA__11,
      "Transformation de viandes (pâtés, salaisons, conserves…)" = TransformationPA__12,
      "Découpe de viandes, caissettes…" = TransformationPA__13,
      "Production de produits à base de miel" = TransformationPA__14,
      "Transformation d’autres produits agricoles (hors aliments pour les animaux de l’exploitation)" = TransformationPA__15,
      "Préparation de plats cuisinés" = TransformationPA__16,
      "Aucune transformation réalisée sur l'exploitation" = TransformationPA__17
    ) |>
    select(-starts_with("TransformationPA")),
  by = "interview__key"
)
writeCSV(TCD23)

# TCD 24 à 33 : Répartition de la destination des produits (Ex : nombre d'exploitants autoconsommant entre 0 et 25% de leur production maraichère)
source("TCD/ExportsTCD_IleCommune_Destination.R")

# TCD 34 à 40 : Pourcentage de destination des produits de culture (Ex : % de la surface maraîchère est auto-consommée)
source("TCD/ExportsTCD_IleCommune_PourcentageDestination.R")

# TCD 41 : Cocoteraies (statut et baguage)
TCD41 <- inner_join(
  rga23_champ_Ile_Commune,
  readCSV("rga23_cocoteraies.csv") |>
    mutate(
      `Cocoteraies baguées` = ifelse(cocoteraieBaguee == 1, 1, 0),
      `Cocoteraies non baguées` = ifelse(cocoteraieBaguee == 2, 1, 0),
      `Cocoteraies en partie baguées` = ifelse(cocoteraieBaguee == 3, 1, 0),
      `Cocoteraies exploitées en tant que Propriétaire plein` = ifelse(statutCoco == 1, 1, 0),
      `Cocoteraies exploitées en tant que Propriétaire en indivision` = ifelse(statutCoco == 2, 1, 0),
      `Cocoteraies exploitées en tant qu'Exploitant` = ifelse(statutCoco == 3, 1, 0),
      `Cocoteraies pour lesquelles tout le revenu a été conservé` = case_when(
        ToutRevenu == 1 ~ 1,
        ToutRevenu == 2 ~ 0
      ),
      `Nombre de mois` = case_when(
        is.na(DureeExploit) ~ 12,
        TRUE ~ DureeExploit
      ),
      `Statut du producteur` = case_when(
        statutCoco == 1 ~ "Propriétaire plein",
        statutCoco == 2 ~ "Propriétaire en indivision",
        statutCoco == 3 ~ "Exploitant",
        TRUE ~ "?!?"
      )
    ),
  by = "interview__key"
) |>
  select(
    interview__key,
    Archipel_1,
    Ile,
    Commune,
    starts_with("Cocoteraies"),
    `Statut du producteur`,
    `Nombre de mois`
  )
writeCSV(TCD41)

# TCD 42 : Producteurs de coprah (regénération et entretien des cocoteraies)
TCD42 <- inner_join(
  rga23_champ_Ile_Commune |> filter(ProducteursCoprah == 1),
  readCSV("rga23_coprahculteurs.csv") |>
    mutate(
      `replanté des cocotiers` = case_when(
        ReplanterCocoteraie1 == 1 ~ 1,
        ReplanterCocoteraie2 == 1 ~ 1,
        ReplanterCocoteraie3 == 1 ~ 1,
        TRUE ~ 0
      ),
      `supprimé les anciens cocotiers` = case_when(
        SuppAnciensCocotiers1 == 1 ~ 1,
        SuppAnciensCocotiers2 == 1 ~ 1,
        SuppAnciensCocotiers3 == 1 ~ 1,
        TRUE ~ 0
      ),
      `entretenu les cocoteraies` = case_when(
        EntretienCoco1 > 0 ~ 1,
        EntretienCoco2 > 0 ~ 1,
        EntretienCoco3 > 0 ~ 1,
        TRUE ~ 0
      ),
      `défriché/débroussaillé les cocoteraies` = case_when(
        ModeEntretCoco1__1 == 1 ~ 1,
        ModeEntretCoco2__1 == 1 ~ 1,
        ModeEntretCoco3__1 == 1 ~ 1,
        TRUE ~ 0
      ),
      `fertilisé les cocoteraies (engrais)` = case_when(
        ModeEntretCoco1__2 == 1 ~ 1,
        ModeEntretCoco2__2 == 1 ~ 1,
        ModeEntretCoco3__2 == 1 ~ 1,
        TRUE ~ 0
      ),
      `posé des bagues de cocotiers` = case_when(
        ModeEntretCoco1__3 == 1 ~ 1,
        ModeEntretCoco2__3 == 1 ~ 1,
        ModeEntretCoco3__3 == 1 ~ 1,
        TRUE ~ 0
      ),
      `utilisé des raticides` = case_when(
        RaticidesCoco1 == 1 ~ 1,
        RaticidesCoco2 == 1 ~ 1,
        RaticidesCoco3 == 1 ~ 1,
        TRUE ~ 0
      )
    ) |>
    rename(
      `livré le coprah à un mandataire` = DestinataireLivr__2,
      `livré le coprah à un colleteur/grossiste` = DestinataireLivr__3,
      `livré le coprah à un quelqu'un d'autre` = DestinataireLivr__4
    ),
  by = "interview__key"
) |>
  select(
    interview__key,
    Archipel_1,
    Ile,
    Commune,
    `replanté des cocotiers`,
    `supprimé les anciens cocotiers`,
    `entretenu les cocoteraies`,
    `défriché/débroussaillé les cocoteraies`,
    `fertilisé les cocoteraies (engrais)`,
    `posé des bagues de cocotiers`,
    `utilisé des raticides`,
    starts_with("livré le coprah")
  )
writeCSV(TCD42)
