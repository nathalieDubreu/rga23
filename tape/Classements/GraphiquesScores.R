# install.packages("gridExtra")
library(ggplot2)

chemin <- paste(Sys.getenv("cheminAcces"), "/SortiesR/", sep = "")

plot_diversity_scores <- function(scores, scoreSousCategorie) {
  custom_colors <- c("#4e79a7", "#f28e2b", "#e15759", "#76b7b2")
  
  nbSousCategories <- length(scoreSousCategorie)
  width <- ifelse(nbSousCategories > 1,500,250)
  height <- ifelse(nbSousCategories > 2,400,200)

  png(file = paste0(chemin, gsub("groupesS", "s", deparse(substitute(scores))), ".png"), width = width, height = height)

  plots <- lapply(seq_along(scoreSousCategorie), function(i) {
    ggplot(scores, aes(x = factor(score), y = .data[[scoreSousCategorie[i]]])) +
      geom_bar(stat = "identity", fill = custom_colors[i]) +
      labs(x = "Score", y = "Nombre d'unites", title = gsub("_", "\n", scoreSousCategorie[i])) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
  })

  do.call(gridExtra::grid.arrange, c(plots, ncol = 2))

  dev.off()
}

plot_diversity_scores(groupesScoresDiversite, c("Diversite_1_Culture", "Diversite_2_Animaux", "Diversite_3_Arbres", "Diversite_4_Activite"))
plot_diversity_scores(groupesScoresSynergies, c("Synergies_1_Integration", "Synergies_2_SolPlantes", "Synergies_3_IntegrationArbres", "Synergies_4_Connectivite"))
plot_diversity_scores(groupesScoresEfficience, c("Efficience_1_Intrants", "Efficience_2_Engrais", "Efficience_3_Pesticides", "Efficience_4_ProductiviteBesoins"))
plot_diversity_scores(groupesScoresRecyclage, c("Recyclage_1_BiomasseDechets", "Recyclage_2_Eau", "Recyclage_3_GrainesRaces", "Recyclage_4_Energie"))
plot_diversity_scores(groupesScoresResilience, c("Resilience_1_StabiliteProduction", "Resilience_2_ReductionVulnerabilite"))
plot_diversity_scores(groupesScoresCultureTraditions, c("CultureTraditions_1_regimeAlimentaire"))
plot_diversity_scores(groupesScoresCocreation, c("Cocreation_1_Plateformes", "Cocreation_2_AccesConnaissances", "Cocreation_3_Participation"))
plot_diversity_scores(groupesScoresValeursHumaines, c("ValeursHumaines_2_Travail", "ValeursHumaines_4_BienEtreAnimal"))
plot_diversity_scores(groupesScoresEcoCirculaire, c("EcoCirculaire_1_MarchesLocaux", "EcoCirculaire_2_ReseauxProducteurs", "EcoCirculaire_3_SystAlimLocal"))
plot_diversity_scores(groupesScoresGouvernance, c("Gouvernance_1_Emancipation"))

