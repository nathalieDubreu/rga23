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