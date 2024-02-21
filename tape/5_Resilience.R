# 5. RÉSILIENCE
#
# STABILITÉ DE LA PRODUCTION ET CAPACITÉ À RÉSISTER AUX PERTURBATIONS
# > 0 - Les revenus diminuent d’année en année, la production est très variable malgré un niveau d’intrants constant et il n’y a pas de capacité de récupération après chocs / perturbations.
# > 1 - Les revenus sont sur une tendance à la baisse, la production est variable d’année en année (à intrants constants) et la capacité de récupération après chocs / perturbations est faible.
# > 2 - Le revenu est globalement stable, mais la production est variable d’année en année (à intrants constants). Le revenu et la production se rétablissent en grande partie après des chocs / perturbations.
# > 3 - Le revenu est stable et la production varie peu d’une année à l’autre (à intrants constants). Le revenu et la production se rétablissent en grande partie après des chocs / perturbations.
# > 4 - Le revenu et la production sont stables et augmentent avec le temps. Ils se rétablissent complètement et rapidement après des chocs / perturbations.

## RevenusExpl
# Les revenus de vos productions agricoles sont :
# Stables.........................1
# En diminution avec le temps.....2
# En augmentation avec le temps...3
# Pas de revenu...................4

## CapaciteRecup
# Quelle est votre capacité de rétablissement après les chocs / perturbations (ex : inondation, sécheresse, maladie, ...) ?
# Pas de capacité...1
# Faible capacité...2
# Bonne capacité....3

## EvolutionProduction
# D'année en année, votre production (à intrants constants) est :
# Stable..........1
# Variable........2
# Très variable...3

scoreStabiliteProduction <- rga23_tape |>
  mutate(score = case_when(
    # > 0 - Les revenus diminuent d’année en année, la production est très variable malgré un niveau d’intrants constant et il n’y a pas de capacité de récupération après chocs / perturbations.
    RevenusExpl == 2 & CapaciteRecup == 1 & EvolutionProduction == 3 ~ 0,
    # > 1 - Les revenus sont sur une tendance à la baisse, la production est variable d’année en année (à intrants constants) et la capacité de récupération après chocs / perturbations est faible.
    RevenusExpl == 2 & CapaciteRecup == 2 & EvolutionProduction == 2 ~ 1,
    # > 2 - Le revenu est globalement stable, mais la production est variable d’année en année (à intrants constants). Le revenu et la production se rétablissent en grande partie après des chocs / perturbations.
    RevenusExpl == 1 & CapaciteRecup == 3 & EvolutionProduction == 2 ~ 2,
    # > 3 - Le revenu est stable et la production varie peu d’une année à l’autre (à intrants constants). Le revenu et la production se rétablissent en grande partie après des chocs / perturbations.
    RevenusExpl == 1 & CapaciteRecup == 3 & (EvolutionProduction == 1 | EvolutionProduction == 2) ~ 3,
    # > 4 - Le revenu et la production sont stables et augmentent avec le temps. Ils se rétablissent complètement et rapidement après des chocs / perturbations.
    RevenusExpl == 3 & CapaciteRecup == 3 & EvolutionProduction == 1 ~ 4,
    ## Prise en compte des revenus et de l'évolution de la production uniquement
    RevenusExpl == 2 & EvolutionProduction == 3 ~ 0,
    RevenusExpl == 2 & EvolutionProduction == 2 ~ 1,
    RevenusExpl == 1 & EvolutionProduction == 2 ~ 2,
    RevenusExpl == 1 & (EvolutionProduction == 1 | EvolutionProduction == 2) ~ 3,
    RevenusExpl == 3 & EvolutionProduction == 1 ~ 4,
    TRUE ~ 5
  ))

scoreStabiliteProduction |>
  group_by(score) |>
  count()

test <- scoreStabiliteProduction |>
  filter(score == 5) |>
  group_by(RevenusExpl, EvolutionProduction, CapaciteRecup) |>
  count()
