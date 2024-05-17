library("tinytex")

## Etape 0 : copier ce programme et le renommer

# Etape 1 : décrire les profils et créér la variable utilisée pour grouper les exploitations (variable de profil)

descriptionProfils <- "**Profils considérés :** 4 archipels\\
*(Regroupement des Iles du vent et Iles sous le Vent au sein des Iles de la Société)*\\"

rga23_profil <- 
  rga23_general |>
  mutate(Profil = case_when( 
    Archipel_1 == "Iles Du Vent" ~ "Iles de la Société",
    Archipel_1 == "Iles Sous-Le-Vent" ~ "Iles de la Société",
    TRUE ~ Archipel_1)) |>
  select(interview__key, Profil)

# Lancement des programmes communs quelque soit la variable de profil
source("tape/Profils/FonctionsGraphiques.R")
source("tape/Profils/PreparationTablesGraphiquesRadar.R")

# Etape 2 : 
## Changer le titre du document pour le rendre cohérent avec la variable de profil + le nom du fichier pdf à exporter
titreDansLeDocument <- "Graphiques Radars par Archipel"
nomFichierPDF <- "GraphiquesRadars_ByArchipel.pdf"

# Création du fichier contenant les graphiques 
rmarkdown::render(
  input = "tape/Profils/GraphiquesRadars.Rmd",
  output_file = nomFichierPDF
)
