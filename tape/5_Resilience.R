# 5. RÉSILIENCE
#
# 5.1 STABILITÉ DE LA PRODUCTION ET CAPACITÉ À RÉSISTER AUX PERTURBATIONS
# > 0 - Les revenus diminuent d’année en année, la production est très variable malgré un niveau d’intrants constant et il n’y a pas de capacité de récupération après chocs / perturbations.
# > 1 - Les revenus sont sur une tendance à la baisse, la production est variable d’année en année (à intrants constants) et la capacité de récupération après chocs / perturbations est faible.
# > 2 - Le revenu est globalement stable, mais la production est variable d’année en année (à intrants constants). Le revenu et la production se rétablissent en grande partie après des chocs / perturbations.
# > 3 - Le revenu est stable et la production varie peu d’une année à l’autre (à intrants constants). Le revenu et la production se rétablissent en grande partie après des chocs / perturbations.
# > 4 - Le revenu et la production sont stables et augmentent avec le temps. Ils se rétablissent complètement et rapidement après des chocs / perturbations.

## RevenusExpl
# Les revenus de vos productions agricoles sont :
# Stables.........................1 -> 2
# En diminution avec le temps.....2 -> 1
# En augmentation avec le temps...3 -> 3
# Pas de revenu...................4

## CapaciteRecup
# Quelle est votre capacité de rétablissement après les chocs / perturbations (ex : inondation, sécheresse, maladie, ...) ?
# Pas de capacité...1
# Faible capacité...2
# Bonne capacité....3

## EvolutionProduction
# D'année en année, votre production (à intrants constants) est :
# Stable..........1 -> 3
# Variable........2 -> 2
# Très variable...3 -> 1

score_1_StabiliteProduction <- rga23_tape |>
  mutate(
    ## RevenusExpl
    # Les revenus de vos productions agricoles sont :
    # Stables.........................1 -> 2
    # En diminution avec le temps.....2 -> 1
    # En augmentation avec le temps...3 -> 3
    # Pas de revenu...................4
    RevenusExplCorrige = case_when(
      RevenusExpl == 1 ~ 2,
      RevenusExpl == 2 ~ 1,
      RevenusExpl == 3 ~ 3,
      TRUE ~ NA
    ),
    ## EvolutionProduction
    # D'année en année, votre production (à intrants constants) est :
    # Stable..........1 -> 3
    # Variable........2 -> 2
    # Très variable...3 -> 1)
    EvolutionProductionCorrige = case_when(
      EvolutionProduction == 1 ~ 3,
      EvolutionProduction == 2 ~ 2,
      EvolutionProduction == 3 ~ 1,
      TRUE ~ NA
    ),
    NotePonderee = trunc((RevenusExplCorrige + EvolutionProductionCorrige + CapaciteRecup) / 3 * 5 / 3 - 1)
  ) |>
  mutate(score = case_when(
    ## Pas de revenus -> non concernés
    RevenusExpl == 4 ~ 99,
    ## Pas de production -> non concernés (cas des interruptions temporaires d'activités)
    is.na(EvolutionProduction) ~ 99,
    !is.na(NotePonderee) ~ NotePonderee,
    TRUE ~ 55
  ))

score_1_StabiliteProduction |>
  group_by(score) |>
  count()

# 5.2 MÉCANISMES DE RÉDUCTION DE LA VULNÉRABILITÉ
# Avec perspective de genre
# > 0 - Pas d’accès au crédit, pas d’assurance, pas de mécanismes de soutien communautaire.
# > 1 - La communauté n’est pas très favorable et sa capacité à aider après les chocs est très limitée. Et / ou l’accès au crédit et à l’assurance est limité.
# > 2 - La communauté est solidaire mais sa capacité à aider après les chocs est limitée. Et / ou l’accès au crédit est disponible mais difficile à obtenir en pratique. L’assurance est rare et ne permet pas une couverture complète contre les risques.
# > 3 - La communauté est très solidaire aux hommes et aux femmes, mais sa capacité à aider après les chocs est limitée. Et / ou l’accès au crédit est disponible et l’assurance ne couvre que des produits / risques spécifiques.
# > 4 - La communauté soutient fortement les hommes et les femmes et peut apporter une aide significative après les chocs. Et / ou l’accès au crédit est quasi systématique et l’assurance couvre l’essentiel de la production.

score_2_ReductionVulnerabilite <- rga23_tape |>
  mutate(
    nombreAidesProjets = rowSums(across(
      all_of(starts_with("AideProjets_")),
      ~ coalesce(., 0)
    )),
    score = case_when(
      # Non à tout (8)
      AideProjets__8 == 1 ~ 0,
      # Accès uniquement au 2
      AideProjets__2 == 1 & (nombreAidesProjets == 1) ~ 1,
      # Accès uniquement aux 3 et/ou 7 (+ éventuellement le 2)
      (AideProjets__3 == 1 | AideProjets__7 == 1) &
        (nombreAidesProjets == (AideProjets__2 + AideProjets__3 + AideProjets__7)) ~ 2,
      # Au moins deux parmi 1, 3, 5 et 6 mais pas 4
      AideProjets__4 == 0 &
        ((AideProjets__1 + AideProjets__3 + AideProjets__5 + AideProjets__6) >= 2) ~ 3,
      # Au moins le 4
      AideProjets__4 == 1 ~ 4,
      TRUE ~ 55
    )
  )

score_2_ReductionVulnerabilite |>
  group_by(score) |>
  count()
