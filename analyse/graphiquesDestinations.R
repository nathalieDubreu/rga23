donnees_summary_DHV <- as.data.frame(prop.table(table(tableMaraichage$TailleExploitation, tableMaraichage$DestinationsHorsVente), margin = 1))
colnames(donnees_summary_DHV) <- c("TailleExploitation", "DestinationsHorsVente", "ProportionDHV")

donnees_summary_VD <- as.data.frame(prop.table(table(tableMaraichage$TailleExploitation, tableMaraichage$VenteDirecte), margin = 1))
colnames(donnees_summary_VD) <- c("TailleExploitation", "VenteDirecte", "ProportionVD")

donnees_summary_VP <- as.data.frame(prop.table(table(tableMaraichage$TailleExploitation, tableMaraichage$VenteAuxProfessionnels), margin = 1))
colnames(donnees_summary_VP) <- c("TailleExploitation", "VenteAuxProfessionnels", "ProportionVP")

# Maraichage - Destination Hors vente
ggplot(donnees_summary_DHV, aes(x = TailleExploitation, y = ProportionDHV, fill = DestinationsHorsVente)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  labs(x = "Taille Exploitation", y = "Proportion", fill = "DestinationsHorsVente", title = "MARAICHAGE - Part de destination hors vente par taille Exploitation")
