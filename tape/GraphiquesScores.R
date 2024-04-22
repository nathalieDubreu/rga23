# install.packages("gridExtra")
library(ggplot2)

chemin <- paste(Sys.getenv("cheminAcces"), "/SortiesR/", sep="")

plot_diversity_scores <- function(scores, scoreSousCategorie) {
  custom_colors <- c("#4e79a7", "#f28e2b", "#e15759", "#76b7b2")
  
  png(file = paste0(chemin, deparse(substitute(scores)), ".png"), width = 500, height = 400)
  
  plots <- lapply(seq_along(scoreSousCategorie), function(i) {
    ggplot(scores, aes(x = factor(score), y = .data[[scoreSousCategorie[i]]])) +
      geom_bar(stat = "identity", fill = custom_colors[i]) +
      labs(x = "Score", y = "Nombre d'unités", title = paste("Scores", scoreSousCategorie[i])) +
      theme_minimal()
  })
  
  do.call(gridExtra::grid.arrange, c(plots, ncol = 2))
  
  dev.off()
}

plot_diversity_scores(scoresDiversite, c("Diversite_1_Culture", "Diversite_2_Animaux", "Diversite_3_Arbres", "Diversite_4_Activite"))
plot_diversity_scores(scoresSynergies, c("Synergies_1_Integration", "Synergies_2_SolPlantes", "Synergies_3_IntegrationArbres", "Synergies_4_Connectivite"))
plot_diversity_scores(scoresEfficience, c("Efficience_1_Intrants", "Efficience_2_Engrais", "Efficience_3_Pesticides", "Efficience_4_ProductiviteBesoins"))
plot_diversity_scores(scoresRecyclage, c("Recyclage_1_BiomasseDechets", "Recyclage_2_Eau", "Recyclage_3_GrainesRaces", "Recyclage_4_Energie"))
plot_diversity_scores(scoresCocreation, c("Cocreation_1_Plateformes", "Cocreation_2_AccesConnaissances", "Cocreation_3_Participation"))

