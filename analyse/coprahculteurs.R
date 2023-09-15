source("analyse/eligibles.R")

## Coprahculteurs éligibles
eligiblesCoprahculteurs <- eligiblesRGA |>
  filter(RaisonsRecensement__3 == 1)

aVerifier <- eligiblesCoprahculteurs |> filter(ProprieteSechoir__1 == 1 & ProprieteSechoir__2 == 1)

cocoteraiesExploitees <- eligiblesCoprahculteurs |> summarize(`Nb de producteurs de coprah` = n(), 
                                     `Nb de cocoteraies exploitées` = sum(NbCocoteraies),  
                                     `- en tant que propriétaire plein` = sum(nbCocoStatut1),
                                     `- en tant que propriétaire en indivision` = sum(nbCocoStatut2),
                                     `- en tant qu'exploitant` = sum(nbCocoStatut3),
                                     `Nb moyen de cocoteraies` = mean(NbCocoteraies),
                                     `Nb min de cocoteraies` = min(NbCocoteraies),
                                     `Nb max de cocoteraies` = max(NbCocoteraies))

writeCSV(cocoteraiesExploitees)
