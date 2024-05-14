library("tinytex")

## Etape 0 : copier ce programme et le renommer

# Etape 1 : créér la variable utilisée pour grouper les exploitations (variable de profil) 
## Ici, c'est un regroupement des archipels des Iles du Vent et Iles sous le Vent en Iles de la société dans la variable Archipel_1

rga23_profil <- 
  rga23_general |>
  mutate(Profil = case_when( 
    Archipel_1 == "Iles Du Vent" ~ "Iles de la Société",
    Archipel_1 == "Iles Sous-Le-Vent" ~ "Iles de la Société",
    TRUE ~ Archipel_1)) |>
  select(interview__key, Profil)

source("tape/Profils/PreparationTablesGraphiquesRadar.R")

# Etape 2 : changer le titre pour le rendre cohérent avec la variable de profil
rmarkdown::render("tape/Profils/GraphiquesRadars_ByArchipel.Rmd")
