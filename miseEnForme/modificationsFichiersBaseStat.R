## Récupération des données rga23_gestion dans le rga23_general
rga23_general <- left_join(rga23_general, rga23_gestion)

## Nouvelle répartition des données entre rga23_general et rga23_mainOeuvre

listeVariablesADeplacer <- c(
  "Archipel",
  "Ile",
  "Commune",
  "PresNumTahiti",
  "NumeroTahiti",
  "SexeChefExpl",
  "DateNaissChefExpl",
  "SituationConjChefExpl",
  "ActiviteConjoint__1",
  "ActiviteConjoint__2",
  "ActiviteConjoint__3",
  "ActiviteConjoint__4",
  "ActiviteConjoint__5",
  "ActiviteConjoint__6",
  "ActiviteConjoint__7",
  "ActiviteConjoint__8",
  "ActiviteConjoint__9",
  "ActiviteConjoint__10",
  "ActiviteConjoint__11",
  "ActiviteConjoint__12",
  "SalarieExplChefExpl",
  "AnneeInstallationChefExpl",
  "repriseExploit",
  "FormNAChefExpl",
  "FormAgriChefExpl",
  "FormationContinue",
  "DureeFormationsContinues",
  "FormRecentesChefExpl",
  "ActivitesChefExploit__1",
  "ActivitesChefExploit__2",
  "ActivitesChefExploit__3",
  "ActivitesChefExploit__4",
  "ActivitesChefExploit__5",
  "ActivitesChefExploit__6",
  "ActivitesChefExploit__7",
  "ActivitesChefExploit__8",
  "ActivitesChefExploit__9",
  "ActivitesChefExploit__10",
  "ActivitesChefExploit__11",
  "ActivitesChefExploit__12",
  "AutreActivite",
  "ActivitePrincipaleChef",
  "TransformationPA__1",
  "TransformationPA__2",
  "TransformationPA__3",
  "TransformationPA__4",
  "TransformationPA__5",
  "TransformationPA__6",
  "TransformationPA__7",
  "TransformationPA__8",
  "TransformationPA__9",
  "TransformationPA__10",
  "TransformationPA__11",
  "TransformationPA__12",
  "TransformationPA__13",
  "TransformationPA__14",
  "TransformationPA__15",
  "TransformationPA__16",
  "TransformationPA__17",
  "TransformationCoco__1",
  "TransformationCoco__2",
  "TransformationCoco__3",
  "TransformationCoco__4",
  "TransformationCoco__5",
  "TransformationCoco__6",
  "TransformationCoco__7",
  "TransformationCoco__8"
)
rga23_mainOeuvre <- left_join(rga23_mainOeuvre, rga23_general |>
  select("interview__key", all_of(listeVariablesADeplacer)))

rga23_general <- rga23_general |>
  select(!all_of(listeVariablesADeplacer))

writeCSVTraites(rga23_general)
writeCSVTraites(rga23_mainOeuvre)

# Suppression du rga23_gestion
removeCSVTraites(rga23_gestion)
rm(rga23_gestion)
