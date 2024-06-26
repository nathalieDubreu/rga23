# install.packages("FactoMineR")
# install.packages("factoextra")
library(FactoMineR)
library(factoextra)
library(ggplot2)

## Champ : 4080 exploitations au sens du RGA 2023

## Restriction au champ
rga23_champ <- left_join(readCSV("rga23_general.csv") |> filter(indicRGA23 == 1),
  readCSV("rga23_exploitations.csv") |> select(interview__key, eligibilite),
  by = "interview__key"
) |>
  left_join(
    readCSV("rga23_coprahculteurs.csv") |> select(interview__key, eligibiliteCoprah),
    by = "interview__key"
  ) |>
  mutate(
    Cultivateurs = ifelse(indicRGA23 == 1 & RaisonsRecensement__1 == 1 & eligibilite == 1, 1, 0),
    Eleveurs = ifelse(indicRGA23 == 1 & RaisonsRecensement__2 == 1 & eligibilite == 1, 1, 0),
    ProducteursCoprah = ifelse(indicRGA23 == 1 & RaisonsRecensement__3 == 1 & eligibiliteCoprah == 1, 1, 0),
  ) |>
  select(-starts_with("indic"), -starts_with("eligibilite"), -statut_collecte, -starts_with("RaisonsRecensement"), -PointsCAPL, -ActiviteEnquete)

variablesNecessaires <- left_join(
  rga23_champ,
  readCSV("rga23_tousFichiersPlats.csv") |>
    select(
      interview__key,
      starts_with("PresenceAnimaux__"),
      starts_with("CulturesPresentes__"),
      SurfaceJardins,
      ActivitesChefExploit__5
    ),
  by = "interview__key"
)

## Mise en avant des combinaisons d'activités
combinaisonsActivites <- variablesNecessaires |>
  mutate(
    # Cultures maraîchères....................................10/10
    activite_A = ifelse(replace_na(CulturesPresentes__10, 0) == 1, " Maraichage /", "-/"),
    # Cultures vivrières......................................20 /20
    activite_B = ifelse(replace_na(CulturesPresentes__20, 0) == 1, " Vivrier /", "-/"),
    # Cultures fruitières (hors pépinères) et bois d'oeuvre...30 /30
    activite_C = ifelse(replace_na(CulturesPresentes__30, 0) == 1, " Fruitier /", "-/"),
    # Feuillages et cultures florales (hors pépinières).......40 /40
    activite_D = ifelse(replace_na(CulturesPresentes__40, 0) == 1, " Floral /", "-/"),
    # Plantes aromatiques, stimulantes et médicinales.........50 /50
    activite_E = ifelse(replace_na(CulturesPresentes__50, 0) == 1, " Ppam /", "-/"),
    # Pépinières (plantes vendues en pot).....................60 /60
    activite_F = ifelse(replace_na(CulturesPresentes__60, 0) == 1, " Pepinieres /", "-/"),
    # Cultures fourragères....................................70 /70
    activite_G = ifelse(replace_na(CulturesPresentes__70, 0) == 1, " Fourrageres /", "-/"),
    # Jachères................................................80 /80
    activite_H = ifelse(replace_na(CulturesPresentes__80, 0) == 1, " Jacheres /", "-/"),
    # Jardins océaniens
    activite_I = ifelse(replace_na(SurfaceJardins, 0) > 0, " JardinsOceaniens /", "-/"),
    # Bovins..................................................1 /1
    activite_J = ifelse(replace_na(PresenceAnimaux__1, 0) == 1, " Bovins /", "-/"),
    # Ovins...................................................2 /2
    activite_K = ifelse(replace_na(PresenceAnimaux__2, 0) == 1, " Ovins /", "-/"),
    # Porcins.................................................3 /3
    activite_L = ifelse(replace_na(PresenceAnimaux__3, 0) == 1, " Porcins /", "-/"),
    # Volailles...............................................4 /4
    activite_M = ifelse(replace_na(PresenceAnimaux__4, 0) == 1, " Volailles /", "-/"),
    # Equidés.................................................5 /5
    activite_N = ifelse(replace_na(PresenceAnimaux__5, 0) == 1, " Equides /", "-/"),
    # Lapins élevés pour la chair.............................6 /6
    activite_O = ifelse(replace_na(PresenceAnimaux__6, 0) == 1, " Lapins /", "-/"),
    # Abeilles................................................7 /7
    activite_P = ifelse(replace_na(PresenceAnimaux__7, 0) == 1, " Abeilles /", "-/"),
    # Caprins.................................................8 /8
    activite_Q = ifelse(replace_na(PresenceAnimaux__8, 0) == 1, " Caprins /", "-/"),
    # Peche
    activite_R = ifelse(replace_na(ActivitesChefExploit__5, 0) == 1, " Peche /", "-/"),
    # Coprah
    activite_S = ifelse(replace_na(ProducteursCoprah, 0) == 1, " Coprah", "-"),
    # Concaténation
    Activites = paste0(
      activite_A, activite_B, activite_C, activite_D, activite_E, activite_F,
      activite_G, activite_H, activite_I, activite_J, activite_K, activite_L,
      activite_M, activite_N, activite_O, activite_P, activite_Q, activite_R, activite_S
    )
  ) |>
  select(interview__key, Archipel_1, Activites)

comptagesCombinaisonsActivites <- combinaisonsActivites |>
  group_by(Activites) |>
  summarize(`Nombre d'exploitants` = n()) |>
  arrange(desc(`Nombre d'exploitants`))

writeCSV(comptagesCombinaisonsActivites)

## Zoom sur les combinaisons d'activités qui concernent au moins 25 chefs d'exploitations ou coprahculteurs

comptagesFiltres <- comptagesCombinaisonsActivites |>
  filter(`Nombre d'exploitants` >= 25)

combinaisonsActivitesFiltrees <- combinaisonsActivites |>
  semi_join(comptagesFiltres, by = "Activites") |>
  select(-Archipel_1)

qqStatsParCombinaisonActivite <- rga23_champ |>
  select(interview__key, Archipel_1) |>
  left_join(combinaisonsActivitesFiltrees,
    by = "interview__key"
  ) |>
  mutate(Activites = case_when(
    !is.na(Activites) ~ Activites,
    TRUE ~ "Autres combinaisons"
  )) |>
  left_join(
    readCSV("rga23_mainOeuvre.csv") |> mutate(
      age = 2023 - as.numeric(substring(DateNaissChefExpl, 7, 10)),
      homme = case_when(SexeChefExpl == 1 ~ 0, SexeChefExpl == 2 ~ 1),
      femme = case_when(SexeChefExpl == 1 ~ 1, SexeChefExpl == 2 ~ 0)
    ) |>
      select(interview__key, homme, femme, age),
    by = "interview__key"
  ) |>
  left_join(
    readCSV("rga23_etp.csv"),
    by = "interview__key"
  ) |>
  left_join(
    readCSV("rga23_prodVegetales.csv") |> select(interview__key, SurfaceTotalProdAgri, SurfaceProdVegetales_HC_HP),
    by = "interview__key"
  ) |>
  left_join(
    readCSV("rga23_prodAnimales.csv") |>
      mutate(
        EleveurBovins = PresenceAnimaux__1,
        EleveurOvins = PresenceAnimaux__2,
        EleveurPorcins = PresenceAnimaux__3,
        EleveurVolailles = PresenceAnimaux__4,
        EleveurPoulesPondeuses = ifelse(PresenceAnimaux__4 == 1 & (TypeVolailles__1 == 1 | TypeVolailles__3 == 1 | TypeVolailles__4 == 1), 1, 0),
        EleveurEquides = PresenceAnimaux__5,
        EleveurLapins = PresenceAnimaux__6,
        Apiculteurs = PresenceAnimaux__7,
        EleveurCaprins = PresenceAnimaux__8,
        NombreBovins = replace_na(nbTotalBovins, 0),
        NombreOvins = replace_na(nbTotalOvins, 0),
        NombrePorcins = replace_na(nbTotalPorcs, 0),
        NombreVolailles = rowSums(across(
          c("NbAutresVolailles", "NbDindesDindons", "NbOies", "NbCanards", "NbCailles", "NbPintades", "NbPouletsChairCoqs", "NbPoulettes", "NbPoussins", "NombrePoules0", "NombrePoules1", "NombrePoules3"),
          ~ (coalesce(., 0))
        )),
        NombrePoulesPondeuses = rowSums(across(
          c("NombrePoules0", "NombrePoules1", "NombrePoules3"),
          ~ (coalesce(., 0))
        )),
        NombreEquides = replace_na(nbTotalEquides, 0),
        NombreLapins = rowSums(across(
          c("NbLapereaux", "NbLapinesFutures", "NbLapinesMeres", "NbLapinsReprod", "NbLapinsSevresEngrais"),
          ~ (coalesce(., 0))
        )),
        NombreRuchesPourProduire = replace_na(NbRuchesPourProduire, 0),
        NombreCaprins = replace_na(nbTotalCaprins, 0)
      ) |>
      select(
        interview__key,
        starts_with("Eleveur"),
        Apiculteurs,
        NombreBovins,
        NombreOvins,
        NombrePorcins,
        NombreVolailles,
        NombrePoulesPondeuses,
        NombreEquides,
        NombreLapins,
        NombreRuchesPourProduire,
        NombreCaprins
      ),
    by = "interview__key"
  )
writeCSV(qqStatsParCombinaisonActivite)

###################
#        ACP      #
###################

tableACP <- variablesNecessaires |>
  mutate(
    Maraichage = replace_na(CulturesPresentes__10, 0),
    Vivrier = replace_na(CulturesPresentes__20, 0),
    Fruitier = replace_na(CulturesPresentes__30, 0),
    Floral = replace_na(CulturesPresentes__40, 0),
    Ppam = replace_na(CulturesPresentes__50, 0),
    Pepinieres = replace_na(CulturesPresentes__60, 0),
    Fourrageres = replace_na(CulturesPresentes__70, 0),
    Jacheres = replace_na(CulturesPresentes__80, 0),
    JardinsOceaniens = ifelse(replace_na(SurfaceJardins, 0) > 0, 1, 0),
    Bovins = replace_na(PresenceAnimaux__1, 0),
    Ovins = replace_na(PresenceAnimaux__2, 0),
    Porcins = replace_na(PresenceAnimaux__3, 0),
    Volailles = replace_na(PresenceAnimaux__4, 0),
    Equides = replace_na(PresenceAnimaux__5, 0),
    Lapins = replace_na(PresenceAnimaux__6, 0),
    Abeilles = replace_na(PresenceAnimaux__7, 0),
    Caprins = replace_na(PresenceAnimaux__8, 0),
    Peche = replace_na(ActivitesChefExploit__5, 0),
    Coprah = replace_na(ProducteursCoprah, 0)
  ) |>
  select(
    Maraichage, Vivrier, Fruitier, Floral, Ppam, Pepinieres, Fourrageres, Jacheres,
    JardinsOceaniens,
    Bovins, Ovins, Porcins, Volailles, Equides, Lapins, Abeilles, Caprins,
    Peche,
    Coprah
  )

result <- PCA(tableACP, graph = FALSE)

## Matrice de corrélations
matriceCorrelations <- as.data.frame(cor(tableACP, use = "pairwise.complete.obs"))
writeCSV(matriceCorrelations)

## Graphique des variables
fviz_pca_var(result,
  col.var = "contrib",
  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
  repel = TRUE
)

## Graphique des individus
fviz_pca_ind(result,
  col.ind = "cos2",
  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
  repel = TRUE
)
### Pas très lisible...

## Regroupements d'exploitations ?!?

## Méthode du coude pour déterminer combien de regroupements choisir
set.seed(123)
coordonneesExploitations <- result$ind$coord[, 1:2]
wcss <- numeric(10)
for (i in 1:10) {
  kmeans_result <- kmeans(coordonneesExploitations, centers = i, nstart = 25)
  wcss[i] <- kmeans_result$tot.withinss
}
plot(1:10, wcss,
  type = "b", pch = 19, frame = FALSE,
  xlab = "Nombre de clusters", ylab = "WCSS"
)
## -> 3 regroupements ^^

## Nombre de regroupements (clusters)
nbClusters <- 3

kmeans_result <- kmeans(coordonneesExploitations, centers = nbClusters, nstart = 25)

# Résultats des regroupements
coordonneesExploitations$cluster <- factor(kmeans_result$cluster)

# Visualisation
fviz_pca_ind(result,
  geom = "point",
  col.ind = coordonneesExploitations$cluster,
  palette = "jco",
  legend.title = "Clusters"
)

# Ajout des clusters aux données
tableACP$cluster <- coordonneesExploitations$cluster

# Calcul des moyennes des variables par cluster
moyennesParCluster <- tableACP |>
  group_by(cluster) |>
  summarise(across(everything(), mean, na.rm = TRUE))
writeCSV(moyennesParCluster)

# Préparation données pour ggplot
moyennesParClusterBis <- moyennesParCluster |>
  pivot_longer(cols = -cluster, names_to = "variable", values_to = "mean")

# Visualisation
ggplot(moyennesParClusterBis, aes(x = variable, y = mean, fill = cluster)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(title = "Moyennes des Variables par cluster", x = "Variable", y = "Moyenne") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Contenu des clusters
tableACPBis <- tableACP |>
  pivot_longer(cols = c(
    Maraichage, Vivrier, Fruitier, Floral, Ppam, Pepinieres, Fourrageres, Jacheres,
    JardinsOceaniens,
    Bovins, Ovins, Porcins, Volailles, Equides, Lapins, Abeilles, Caprins,
    Peche,
    Coprah
  ), names_to = "variable", values_to = "value")

# Visualisation
ggplot(tableACPBis, aes(x = cluster, fill = as.factor(value))) +
  geom_bar(position = "stack", color = "black") +
  facet_wrap(~ variable, scales = "free_y", ncol = 2) +  
  labs(title = "Variables par cluster",
       x = "Cluster", y = "Nombre d'observations") +
  scale_fill_manual(values = c("#FFA500", "#6495ED"), labels = c("0", "1")) +  
  theme_minimal()

# Calcul des pourcentages de 1 et 0 par variable binaire et par cluster
tableACPPourcentage <- tableACP |>
  group_by(cluster) |>
  summarise(
    across(
      c(
        Maraichage, Vivrier, Fruitier, Floral, Ppam, Pepinieres, Fourrageres, Jacheres,
        JardinsOceaniens, Bovins, Ovins, Porcins, Volailles, Equides, Lapins, Abeilles,
        Caprins, Peche, Coprah
      ),
      ~ mean(. == 1, na.rm = TRUE)
    )
  )

tableACPPourcentageBis <- tableACPPourcentage |>
  pivot_longer(cols = c(
    Maraichage, Vivrier, Fruitier, Floral, Ppam, Pepinieres, Fourrageres, Jacheres,
    JardinsOceaniens, Bovins, Ovins, Porcins, Volailles, Equides, Lapins, Abeilles,
    Caprins, Peche, Coprah
  ), names_to = "variable", values_to = "percentage")

ggplot(tableACPPourcentageBis, aes(x = cluster, y = percentage, fill = variable)) +
  geom_bar(stat = "identity", position = "stack", color = "black") +
  facet_wrap(~ variable, scales = "free_y", ncol = 2) +  
  labs(
    title = "Variables en pourcentage par cluster",
    x = "Cluster", y = "Pourcentage"
  ) +
  theme_minimal() +
  geom_text(aes(label = paste0(round(percentage * 100), "%")),
            position = position_stack(vjust = 0.5), size = 3, color = "white")  

