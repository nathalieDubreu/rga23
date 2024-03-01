## Chefs d'exploitations

ageParArchipel <- rga23_mainOeuvre |>
  group_by(Archipel_1) |>
  summarize(round(mean(age, na.rm = TRUE), 2))
writeCSV(ageParArchipel)

chefsExploitClasseAge <- rga23_mainOeuvre |>
  filter(!is.na(age)) |>
  group_by(`Chefs d'exploitation par classe d'age`) |>
  calculPourcentage()
writeCSV(chefsExploitClasseAge)

chefsExploitClasseAgeArchipel <- rga23_mainOeuvre |>
  filter(!is.na(age)) |>
  groupByTotalEtPourcent(Archipel_1, `Chefs d'exploitation par classe d'age`)
writeCSV(chefsExploitClasseAgeArchipel)

chefsExploitClasseAgeTypeExploitation <- rga23_mainOeuvre |>
  filter(!is.na(age)) |>
  groupByTotalEtPourcent(TypeExploitation, `Chefs d'exploitation par classe d'age`)
writeCSV(chefsExploitClasseAgeTypeExploitation)

genreChef <- rga23_mainOeuvre |>
  mutate(homme = case_when(SexeChefExpl == 1 ~ 0, SexeChefExpl == 2 ~ 1), femme = case_when(SexeChefExpl == 1 ~ 1, SexeChefExpl == 2 ~ 0)) |>
  summarize(
    NbHommes = sum(homme, na.rm = TRUE),
    NbFemmes = sum(femme, na.rm = TRUE),
    TauxFemmes = round(NbFemmes / (NbHommes + NbFemmes) * 100, 1)
  )

genreChefArchipel <- rga23_mainOeuvre |>
  group_by(Archipel_1) |>
  summarize(
    NbHommes = sum(homme, na.rm = TRUE),
    NbFemmes = sum(femme, na.rm = TRUE),
    TauxFemmes = round(NbFemmes / (NbHommes + NbFemmes) * 100, 1)
  ) |>
  select(Archipel_1, TauxFemmes) |>
  add_row(
    Archipel_1 = "Total",
    TauxFemmes = genreChef$TauxFemmes
  )
writeCSV(genreChefArchipel)

genreTypeExploitation <- rga23_mainOeuvre |>
  group_by(TypeExploitation) |>
  summarize(
    NbHommes = sum(homme, na.rm = TRUE),
    NbFemmes = sum(femme, na.rm = TRUE),
    TauxFemmes = round(NbFemmes / (NbHommes + NbFemmes) * 100, 1)
  ) |>
  select(!NbHommes & !NbFemmes) |>
  add_row(
    TypeExploitation = "Total",
    TauxFemmes = genreChef$TauxFemmes
  )
writeCSV(genreTypeExploitation)

tauxFeminisationClasseAge <- rga23_mainOeuvre |>
  filter(!is.na(age)) |>
  group_by(`Chefs d'exploitation par classe d'age`) |>
  summarise(
    NbHommes = sum(homme, na.rm = TRUE),
    NbFemmes = sum(femme, na.rm = TRUE),
    TauxFemmes = round(NbFemmes / (NbHommes + NbFemmes) * 100, 1)
  ) |>
  select(`Chefs d'exploitation par classe d'age`, TauxFemmes)
writeCSV(tauxFeminisationClasseAge)

### Temps de travail du chef d'exploitation

tempsTravailChef <- rga23_mainOeuvre |>
  filter(!is.na(TpsTravailChefExpl)) |>
  mutate(`Temps de travail du chef d'exploitation` = case_when(
    (TpsTravailChefExpl == 1) ~ "1 : Moins de 1/2 temps",
    (TpsTravailChefExpl == 2) ~ "2 : 1/2 temps",
    (TpsTravailChefExpl == 3) ~ "3 : Entre 1/2 temps et temps complet",
    (TpsTravailChefExpl == 4) ~ "4 : Temps complet",
    (is.na(TpsTravailChefExpl)) ~ "Non réponse"
  )) |>
  group_by(`Temps de travail du chef d'exploitation`) |>
  calculPourcentage()

tempsTravailChefArchipel <- rga23_mainOeuvre |>
  filter(!is.na(TpsTravailChefExpl)) |>
  mutate(`Temps de travail du chef d'exploitation` = case_when(
    (TpsTravailChefExpl == 1) ~ "1 : Moins de 1/2 temps",
    (TpsTravailChefExpl == 2) ~ "2 : 1/2 temps",
    (TpsTravailChefExpl == 3) ~ "3 : Entre 1/2 temps et temps complet",
    (TpsTravailChefExpl == 4) ~ "4 : Temps complet",
    (is.na(TpsTravailChefExpl)) ~ "Non réponse"
  )) |>
  groupByTotalEtPourcent(Archipel_1, `Temps de travail du chef d'exploitation`)
writeCSV(tempsTravailChefArchipel)

formNAChefExpl <- rga23_mainOeuvre |>
  filter(!is.na(FormNAChefExpl)) |>
  mutate(`Formation générale non agricole du chef d'exploitation` = case_when(
    (FormNAChefExpl == 1) ~ "1 : Aucune",
    (FormNAChefExpl == 2) ~ "2 : Primaire",
    (FormNAChefExpl == 3) ~ "3 : Secondaire court",
    (FormNAChefExpl == 4) ~ "4 : Secondaire long",
    (FormNAChefExpl == 5) ~ "5 : Supérieure",
    (is.na(FormNAChefExpl)) ~ "Non réponse"
  )) |>
  group_by(`Formation générale non agricole du chef d'exploitation`) |>
  calculPourcentage()

formAgriChefExpl <- rga23_mainOeuvre |>
  filter(!is.na(FormNAChefExpl)) |>
  mutate(`Formation générale agricole du chef d'exploitation` = case_when(
    (FormAgriChefExpl == 1) ~ "1 : Aucune / Formation sur le tas",
    (FormAgriChefExpl == 2) ~ "2 : MFR, CJA sans obtention de diplôme",
    (FormAgriChefExpl == 3) ~ "3 : Secondaire courte (CAPA, BEPA)",
    (FormAgriChefExpl == 4) ~ "4 : Secondaire longue (BTA, Bac agricole)",
    (FormAgriChefExpl == 5) ~ "5 : Supérieure courte (BTSA)",
    (FormAgriChefExpl == 6) ~ "6 : Supérieure longue (ingénieur)",
    (is.na(FormAgriChefExpl)) ~ "Non réponse"
  )) |>
  group_by(`Formation générale agricole du chef d'exploitation`) |>
  calculPourcentage()

## Main d'oeuvre

nbCoexploitants <- rga23_mainOeuvre |> summarize(sum(NbCoExploitants, na.rm = TRUE))

nbMOPermFamiliale <- rga23_mainOeuvre |> summarize(sum(NbMOPermFamiliale, na.rm = TRUE))

nbMOPermNonFamiliale <- (rga23_mainOeuvre |> summarize(sum(nbFemmesNFPerm, na.rm = TRUE))) + (rga23_mainOeuvre |> summarize(sum(nbHommesNFPerm, na.rm = TRUE)))

nbMOOccasionnelle <- rga23_mainOeuvre |> summarize(sum(totalMAOccas, na.rm = TRUE))

## Part de l'agriculture dans les revenus

partRevenusAgriculture <- rga23_tape |>
  filter(!is.na(PartRevenusAgriExpl)) |>
  mutate(`Part de l'agriculture dans les revenus` = case_when(
    (PartRevenusAgriExpl == 1) ~ "1 : 0 à 25%",
    (PartRevenusAgriExpl == 2) ~ "2 : 25 à 50%",
    (PartRevenusAgriExpl == 3) ~ "3 : 50 à 75%",
    (PartRevenusAgriExpl == 4) ~ "4 : Plus de 75%",
    (is.na(PartRevenusAgriExpl)) ~ "Non réponse"
  )) |>
  group_by(`Part de l'agriculture dans les revenus`) |>
  calculPourcentage()

## Tape

# Vos besoins en nourriture et autres produits essentiels sont-ils satisfaits par votre production ou les revenus de votre production agricole ?
besoinsNourritureArchipel <- rga23_tape |>
  filter(!is.na(BesoinsSatisf)) |>
  mutate(`Besoins satisfaits ?` = case_when(
    BesoinsSatisf == 1 ~ "Oui",
    BesoinsSatisf == 2 ~ "Non"
  )) |>
  groupByTotalEtPourcent(Archipel_1, `Besoins satisfaits ?`)
writeCSV(besoinsNourritureArchipel)

# Les revenus de votre production agricole vous permettent-ils de réaliser des économies ?
# Economies
# Oui, de façon régulière.......1
# Oui, de façon occasionnelle...2
# Non...........................3
economiesRevenusArchipel <- rga23_tape |>
  filter(!is.na(BesoinsSatisf)) |>
  mutate(`Economies ?` = case_when(
    Economies == 1 ~ "Oui, de façon régulière",
    Economies == 2 ~ "Oui, de façon occasionnelle",
    Economies == 3 ~ "Non",
    BesoinsSatisf == 2 ~ "Non (besoins non satisfaits)"
  )) |>
  groupByTotalEtPourcent(Archipel_1, `Economies ?`)
writeCSV(economiesRevenusArchipel)

# Les revenus de vos productions agricoles sont :
# RevenusExpl
# Stables.........................1
# En diminution avec le temps.....2
# En augmentation avec le temps...3
# Pas de revenu...................4

revenusProdAgricolesArchipel <- rga23_tape |>
  filter(!is.na(RevenusExpl)) |>
  mutate(`Les revenus de vos productions agricoles sont ?` = case_when(
    RevenusExpl == 1 ~ "Stables",
    RevenusExpl == 2 ~ "En diminution avec le temps",
    RevenusExpl == 3 ~ "En augmentation avec le temps",
    RevenusExpl == 4 ~ "Pas de revenu"
  )) |>
  groupByTotalEtPourcent(Archipel_1, `Les revenus de vos productions agricoles sont ?`)
writeCSV(revenusProdAgricolesArchipel)

# Quelle est votre capacité de rétablissement après les chocs / perturbations (ex : inondation, sécheresse, maladie, ...) ?
# Pas de capacité...1
# Faible capacité...2
# Bonne capacité....3

capaciteRetablissementArchipel <- rga23_tape |>
  filter(!is.na(CapaciteRecup)) |>
  mutate(`Capacité de rétablissement après les chocs ?` = case_when(
    CapaciteRecup == 1 ~ "Pas de capacité",
    CapaciteRecup == 2 ~ "Faible capacité",
    CapaciteRecup == 3 ~ "Bonne capacité"
  )) |>
  groupByTotalEtPourcent(Archipel_1, `Capacité de rétablissement après les chocs ?`)
writeCSV(capaciteRetablissementArchipel)

## Activités du chef d'exploitation

libelles <- c(
  "Travail sur l'exploitation",
  "Exploitant agricole (dans une autre exploitation)",
  "Activité salariée",
  "Commerçant, profession libérale",
  "Pêche",
  "Perliculture ou activité liée à la perle",
  "Agro-tourisme",
  "Artisan",
  "Producteur de coprah / noix de coco",
  "Retraité",
  "Sans activité",
  "Autre"
)
libellesOrdonnes <- factor(libelles, levels = libelles, ordered = TRUE)

ActivitePrincipaleChefArchipel <- rga23_mainOeuvre |>
  filter(!is.na(ActivitePrincipaleChef)) |>
  mutate(ActivitePrincipaleChef = libellesOrdonnes[ActivitePrincipaleChef]) |>
  groupByTotalEtPourcent(Archipel_1, ActivitePrincipaleChef)
writeCSV(ActivitePrincipaleChefArchipel)

variablesActivites <- paste0("ActivitesChefExploit__", 1:12)
pourcentages <- sprintf("%.1f%%", colMeans(rga23_mainOeuvre[variablesActivites], na.rm = TRUE) * 100)
pourcentagesActivites <- data.frame(Variable = libelles, Pourcentage = pourcentages)
writeCSV(pourcentagesActivites)

