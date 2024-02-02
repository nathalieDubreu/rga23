source("analyse/eligibles.R")

## Coprahculteurs éligibles
eligiblesCoprahculteurs <- eligiblesRGA |>
  filter(RaisonsRecensement__3 == 1)

aVerifier <- eligiblesCoprahculteurs |> filter(ProprieteSechoir__1 == 1 & ProprieteSechoir__2 == 1)

# Contrôle pas appliqué... ??
# aVerifier <- eligiblesCoprahculteurs |> filter((ProportionExploit2 == 1 | ProportionExploit2 == 1 | ProportionExploit23 == 1) & NbCocoteraies > 1)

cocoteraiesExploitees <- eligiblesCoprahculteurs |> summarize(
  `Nb de producteurs de coprah` = n(),
  `Nb de cocoteraies exploitées` = sum(NbCocoteraies, na.rm = TRUE),
  `- en tant que propriétaire plein` = sum(nbCocoStatut1, na.rm = TRUE),
  `- en tant que propriétaire en indivision` = sum(nbCocoStatut2, na.rm = TRUE),
  `- en tant qu'exploitant` = sum(nbCocoStatut3, na.rm = TRUE),
  `Nb moyen de cocoteraies` = mean(NbCocoteraies, na.rm = TRUE),
  `Nb min de cocoteraies` = min(NbCocoteraies, na.rm = TRUE),
  `Nb max de cocoteraies` = max(NbCocoteraies, na.rm = TRUE)
)

writeCSV(cocoteraiesExploitees)
