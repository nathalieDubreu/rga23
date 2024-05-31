# Demande CTC via l'ISPF sur la vanille

## 509	Vanillier - ombrage naturel
## 510	Vanillier - serre ou ombrière

rga23_champ <- readCSV("rga23_general.csv") |>
  filter(indicRGA23 == 1) |>
  select(interview__key, Archipel_1)

vanillerOmbrageNaturel <- readCSV("rga23_surfacesCultures.csv") |>
  filter(culture_id == 509) |>
  mutate(CultivateurVanilleOmbrageNaturel = 1) |>
  rename(SurfaceVanilleOmbrageNaturel = SurfaceCult) |>
  select(interview__key, CultivateurVanilleOmbrageNaturel, SurfaceVanilleOmbrageNaturel) |>
  inner_join(rga23_champ, by = "interview__key")

vanillerSerreOmbriere <- readCSV("rga23_surfacesCultures.csv") |>
  filter(culture_id == 510) |>
  mutate(CultivateurVanilleSerreOmbriere = 1) |>
  rename(SurfaceVanilleSerreOmbriere = SurfaceCult) |>
  select(interview__key, CultivateurVanilleSerreOmbriere, SurfaceVanilleSerreOmbriere) |>
  inner_join(rga23_champ, by = "interview__key")

vanille <- full_join(vanillerOmbrageNaturel, vanillerSerreOmbriere, by = c("interview__key", "Archipel_1")) |>
  mutate(
    Cultivateurs2TypesVanilles = case_when(
      CultivateurVanilleOmbrageNaturel == 1 & CultivateurVanilleSerreOmbriere == 1 ~ 1,
      TRUE ~ 0
    ),
    SurfaceVanille = replace_na(SurfaceVanilleOmbrageNaturel, 0) +
      replace_na(SurfaceVanilleSerreOmbriere, 0)
  ) |>
  mutate(across(where(is.numeric), ~ coalesce(., 0)))

vanille <- left_join(vanille,
  readCSV("rga23_mainOeuvre.csv") |>
    mutate(
      `Temps de travail du chef d'exploitation` = case_when(
        (TpsTravailChefExpl == 1) ~ "1 : Moins de 1/2 temps",
        (TpsTravailChefExpl == 2) ~ "2 : 1/2 temps",
        (TpsTravailChefExpl == 3) ~ "3 : Entre 1/2 temps et temps complet",
        (TpsTravailChefExpl == 4) ~ "4 : Temps complet",
        (is.na(TpsTravailChefExpl)) ~ "Non réponse"
      ), `Formation générale non agricole du chef d'exploitation` = case_when(
        (FormNAChefExpl == 1) ~ "1 : Aucune",
        (FormNAChefExpl == 2) ~ "2 : Primaire",
        (FormNAChefExpl == 3) ~ "3 : Secondaire court",
        (FormNAChefExpl == 4) ~ "4 : Secondaire long",
        (FormNAChefExpl == 5) ~ "5 : Supérieure",
        (is.na(FormNAChefExpl)) ~ "Non réponse"
      ), `Formation générale agricole du chef d'exploitation` = case_when(
        (FormAgriChefExpl == 1) ~ "1 : Aucune / Formation sur le tas",
        (FormAgriChefExpl == 2) ~ "2 : MFR, CJA sans obtention de diplôme",
        (FormAgriChefExpl == 3) ~ "3 : Secondaire courte (CAPA, BEPA)",
        (FormAgriChefExpl == 4) ~ "4 : Secondaire longue (BTA, Bac agricole)",
        (FormAgriChefExpl == 5) ~ "5 : Supérieure courte (BTSA)",
        (FormAgriChefExpl == 6) ~ "6 : Supérieure longue (ingénieur)",
        (is.na(FormAgriChefExpl)) ~ "Non réponse"
      ),
      FormationContinue = case_when(
        FormationContinue == 1 ~ 1,
        FormationContinue == 2 ~ 0
      )
    ) |>
    select(
      interview__key,
      `Temps de travail du chef d'exploitation`,
      `Formation générale non agricole du chef d'exploitation`,
      `Formation générale agricole du chef d'exploitation`,
      FormationContinue
    ),
  by = "interview__key"
)

writeCSV(vanille)


## Non pris en compte

vanilleJardinsOceaniens <- inner_join(rga23_champ_Ile_Commune,
  readCSV("rga23_prodVegetales.csv") |> select(interview__key, SurfaceJardins, starts_with("CultPrincipJardins")),
  by = "interview__key"
) |> filter(CultPrincipJardins__0 == 509 |
  CultPrincipJardins__0 == 510 |
  CultPrincipJardins__1 == 509 |
  CultPrincipJardins__1 == 510 |
  CultPrincipJardins__2 == 509 |
  CultPrincipJardins__2 == 510 |
  CultPrincipJardins__3 == 509 |
  CultPrincipJardins__3 == 510 |
  CultPrincipJardins__4 == 509 |
  CultPrincipJardins__4 == 510)
