library("tinytex")

## Etape 0 : copier ce programme et le renommer

# Etape 1 : créér la variable utilisée pour grouper les exploitations (variable de profil) 
## Ici : regroupement des Iles du Vent et Iles sous le Vent en Iles de la société dans la variable Archipel_1
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
## Changer le titre du document pour le rendre cohérent avec la variable de profil
titreDocument <- "Graphiques Radars par Archipel"
## Copier-coller le Rmd et le renommer
rmarkdown::render("tape/Profils/GraphiquesRadars_ByArchipel.Rmd")
