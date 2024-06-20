library("tinytex")

## Etape 0 : copier ce programme et le renommer

# Etape 1 : décrire les profils et créér la variable utilisée pour grouper les exploitations (variable de profil)

descriptionProfils <- "**Profils considérés :**\\
*[BLABLABLA]*\\"

rga23_profil <-
  rga23_general |>
  mutate(Profil = case_when(
    # [TODO : typologie à définir]
    TRUE ~ "?!?"
  )) |>
  select(interview__key, Profil)

# Lancement des programmes communs quelque soit la variable de profil
source("tape/Profils/FonctionsGraphiques.R")
source("tape/Profils/PreparationTablesGraphiquesRadar.R")

# Etape 2 :
## Changer le titre du document pour le rendre cohérent avec la variable de profil + le nom du fichier pdf à exporter
titreDansLeDocument <- "Graphiques Radars par BLABLABLA"
nomFichierPDF <- "GraphiquesRadars_BLABLABLA.pdf"

# Création du fichier contenant les graphiques
rmarkdown::render(
  input = "tape/Profils/GraphiquesRadars.Rmd",
  output_file = nomFichierPDF
)
