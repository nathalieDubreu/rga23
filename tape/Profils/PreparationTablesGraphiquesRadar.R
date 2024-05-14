# DIVERSITE

resultatsDiversiteParProfil <- moyenneParProfil(
  scoresDiversite,
  c("Diversite_1_Culture", "Diversite_2_Animaux", "Diversite_3_Arbres", "Diversite_4_Activite"),
  Profil
)
diversiteSansVariableProfil <- ajoutMaxMinTable(resultatsDiversiteParProfil, "Diversite ", Profil)

# SYNERGIE

resultatsSynergiesParProfil <- moyenneParProfil(
  scoresSynergies,
  c("Synergies_1_Integration", "Synergies_2_SolPlantes", "Synergies_3_IntegrationArbres", "Synergies_4_Connectivite"),
  Profil
)
synergieSansVariableProfil <- ajoutMaxMinTable(resultatsSynergiesParProfil, "Synergies ", Profil)

# EFFICIENCE

resultatsEfficienceParProfil <- moyenneParProfil(
  scoresEfficience,
  c("Efficience_1_Intrants", "Efficience_2_Engrais", "Efficience_3_Pesticides", "Efficience_4_ProductiviteBesoins"),
  Profil
)
efficienceSansVariableProfil <- ajoutMaxMinTable(resultatsEfficienceParProfil, "Efficience ", Profil)

head(scoresEfficience)

# RECYCLAGE

resultatsRecyclageParProfil <- moyenneParProfil(
  scoresRecyclage,
  c("Recyclage_1_BiomasseDechets", "Recyclage_2_Eau", "Recyclage_3_GrainesRaces", "Recyclage_4_Energie"),
  Profil
)
recyclageSansVariableProfil <- ajoutMaxMinTable(resultatsRecyclageParProfil, "Recyclage ", Profil)
