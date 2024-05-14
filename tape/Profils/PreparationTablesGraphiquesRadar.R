# DIVERSITE

resultatsDiversiteParProfil <- moyenneParProfil(
  scoresDiversite,
  c("Diversite_1_Culture", "Diversite_2_Animaux", "Diversite_3_Arbres", "Diversite_4_Activite"),
  Profil
)
diversiteSansVariableProfil <- ajoutMaxMinTable(resultatsDiversiteParProfil, Profil)

# SYNERGIE

resultatsSynergiesParProfil <- moyenneParProfil(
  scoresSynergies,
  c("Synergies_1_Integration", "Synergies_2_SolPlantes", "Synergies_3_IntegrationArbres", "Synergies_4_Connectivite"),
  Profil
)
synergieSansVariableProfil <- ajoutMaxMinTable(resultatsSynergiesParProfil, Profil)

# EFFICIENCE

resultatsEfficienceParProfil <- moyenneParProfil(
  scoresEfficience,
  c("Efficience_1_Intrants", "Efficience_2_Engrais", "Efficience_3_Pesticides", "Efficience_4_ProductiviteBesoins"),
  Profil
)
efficienceSansVariableProfil <- ajoutMaxMinTable(resultatsEfficienceParProfil, Profil)

head(scoresEfficience)

# RECYCLAGE

resultatsRecyclageParProfil <- moyenneParProfil(
  scoresRecyclage,
  c("Recyclage_1_BiomasseDechets", "Recyclage_2_Eau", "Recyclage_3_GrainesRaces", "Recyclage_4_Energie"),
  Profil
)
recyclageSansVariableProfil <- ajoutMaxMinTable(resultatsRecyclageParProfil, Profil)

# DIVERS 1 - Résilience / Culture et traditions / Valeurs Humaines / Gouvernance

resultatsResilienceParProfil <- moyenneParProfil(
  scoresResilience,
  c("Resilience_1_StabiliteProduction", "Resilience_2_ReductionVulnerabilite"),
  Profil
)
resilienceSansVariableProfil <- ajoutMaxMinTable(resultatsResilienceParProfil, Profil)

resultatsCultureTradParProfil <- moyenneParProfil(
  scoresCultureTraditions,
  c("CultureTraditions_1_regimeAlimentaire"),
  Profil
)
cultureTraditionsSansVariableProfil <- ajoutMaxMinTable(resultatsCultureTradParProfil, Profil)

resultatsValeursHumainesParProfil <- moyenneParProfil(
  scoresValeursHumaines,
  c("ValeursHumaines_2_Travail", "ValeursHumaines_4_BienEtreAnimal"),
  Profil
)
valeursHumainesSansVariableProfil <- ajoutMaxMinTable(resultatsValeursHumainesParProfil, Profil)

resultatsGouvernanceParProfil <- moyenneParProfil(
  scoresGouvernance,
  c("Gouvernance_1_Emancipation"),
  Profil
)
gouvernanceSansVariableProfil <- ajoutMaxMinTable(resultatsGouvernanceParProfil, Profil)

resultatsDivers1AParProfil <- left_join(resultatsResilienceParProfil,
  resultatsCultureTradParProfil,
  by = c("Profil", "nbExploitations")
)

resultatsDivers1BParProfil <- left_join(resultatsValeursHumainesParProfil, resultatsGouvernanceParProfil,
  by = c("Profil", "nbExploitations")
)

resultatsDivers1ParProfil <- left_join(resultatsDivers1AParProfil,
  resultatsDivers1BParProfil,
  by = c("Profil", "nbExploitations")
)

divers1SansVariableProfil <- ajoutMaxMinTable(resultatsDivers1ParProfil, Profil)


# DIVERS 2 - Co-création / Economie circulaire

resultatsCocreationParProfil <- moyenneParProfil(
  scoresCocreation,
  c("Cocreation_1_Plateformes", "Cocreation_2_AccesConnaissances", "Cocreation_3_Participation"),
  Profil
)
cocreationSansVariableProfil <- ajoutMaxMinTable(resultatsCocreationParProfil, Profil)

resultatsEcoCirculaireParProfil <- moyenneParProfil(
  scoresEcoCirculaire,
  c("EcoCirculaire_1_MarchesLocaux", "EcoCirculaire_2_ReseauxProducteurs", "EcoCirculaire_3_SystAlimLocal"),
  Profil
)
ecoCirculaireSansVariableProfil <- ajoutMaxMinTable(resultatsEcoCirculaireParProfil, Profil)

resultatsDivers2ParProfil <- left_join(resultatsCocreationParProfil,
  resultatsEcoCirculaireParProfil,
  by = c("Profil", "nbExploitations")
)
divers2SansVariableProfil <- ajoutMaxMinTable(resultatsDivers2ParProfil, Profil)
