# Partie 6 - Fertilisation et protection des cultures

## Utilisation produits phyto
produitsPhyto <- rga23_exploitations |>
  filter(eligibilite == 1) |>
  mutate(`Utilisation produits phyto` = case_when(
    UtilisationPhytosanit == 1 ~ "Oui",
    UtilisationPhytosanit == 2 ~ "Non"
  )) |>
  group_by(`Utilisation produits phyto`) |>
  summarise(count = n()) |>
  mutate(`Part des exploitants` = round(count / sum(count) * 100, 1))
Partie6_partExploitantsProduitsPhyto <- produitsPhyto |>
  select(`Utilisation produits phyto`, `Part des exploitants`)
writeCSV(Partie6_partExploitantsProduitsPhyto)

Partie6_nbHa_HC_HP_ExplQuiUtilisentProduitsPhyto <- inner_join(rga23_exploitations |> filter(UtilisationPhytosanit == 1),
  rga23_surfacesCultures_HC_HP,
  by = "interview__key"
) |> summarize(`Nombre d'hectares de productions végétales (hors cocoteraies, hors pâturages) appartenant aux exploitants qui utilisent des produits phytosanitaires` = round(sum(SurfaceCult, rm.na = TRUE) / 10000))
writeCSV(Partie6_nbHa_HC_HP_ExplQuiUtilisentProduitsPhyto)

## par Archipel
Partie6_partUtilisationProduitsPhytoArchipel <- rga23_exploitations |>
  filter(eligibilite == 1) |>
  mutate(`Utilisation produits phyto` = case_when(
    UtilisationPhytosanit == 1 ~ "Oui",
    UtilisationPhytosanit == 2 ~ "Non"
  )) |>
  groupByTotalEtPourcent(Archipel_1, `Utilisation produits phyto`)
writeCSV(Partie6_partUtilisationProduitsPhytoArchipel)

Partie6_typeProduitsPhyto <- rga23_exploitations |>
  filter(UtilisationPhytosanit == 1) |>
  mutate(`Type produits phyto` = case_when(
    TypePhytosanit__1 == 1 & TypePhytosanit__2 == 1 ~ "Synthétique + Biologique",
    TypePhytosanit__1 == 1 ~ "Synthétique (chimique)",
    TypePhytosanit__2 == 1 ~ "Biologique"
  )) |>
  group_by(`Type produits phyto`) |>
  calculPourcentage()
writeCSV(Partie6_typeProduitsPhyto)

## Utilisation engrais
Partie6_partUtilisationEngraisArchipel <- rga23_exploitations |>
  filter(RaisonsRecensement__1 == 1 & eligibilite == 1) |>
  mutate(`Utilisation engrais` = case_when(
    UtilisationEngrais == 1 ~ "Oui",
    UtilisationEngrais == 2 ~ "Non"
  )) |>
  groupByTotalEtPourcent(Archipel_1, `Utilisation engrais`)
writeCSV(Partie6_partUtilisationEngraisArchipel)

# Engrais (ou amendements) de synthèse............1
# Engrais (ou amendements) organiques.............2
# Engrais (ou amendements) minéraux biologiques...3

Partie6_typeEngrais <- rga23_exploitations |>
  filter(UtilisationEngrais == 1) |>
  mutate(`Type engrais` = case_when(
    TypeEngrais__1 == 1 & TypeEngrais__2 == 0 & TypeEngrais__3 == 0 ~ "Engrais (ou amendements) de synthèse - uniquement",
    TypeEngrais__1 == 0 & TypeEngrais__2 == 1 & TypeEngrais__3 == 0 ~ "Engrais (ou amendements) organiques - uniquement",
    TypeEngrais__1 == 0 & TypeEngrais__2 == 0 & TypeEngrais__3 == 1 ~ "Engrais (ou amendements) minéraux biologiques - uniquement",
    TypeEngrais__1 == 1 | TypeEngrais__2 == 1 | TypeEngrais__3 == 1 ~ "Différents types d'engrais",
    TRUE ~ "?!?"
  )) |>
  group_by(`Type engrais`) |>
  calculPourcentage()
writeCSV(Partie6_typeEngrais)

## Epandage des engrais :

Partie6_epandageManuel <- rga23_exploitations |>
  mutate(
    Epandeurs = case_when(
      MaterielEpandage__1 == 1 ~ 1,
      MaterielEpandage__2 == 1 ~ 1,
      MaterielEpandage__3 == 1 ~ 1,
      MaterielEpandage__4 == 1 ~ 1,
      MaterielEpandage__5 == 1 ~ 1,
      MaterielEpandage__6 == 1 ~ 1,
      MaterielEpandage__7 == 1 ~ 1,
      MaterielEpandage__8 == 1 ~ 1,
      TRUE ~ 0
    )
  ) |>
  summarize(
    NbEpandeurs = sum(Epandeurs),
    NbEpandeursManuels = sum(MaterielEpandage__4, na.rm = TRUE),
    ProportionEpandeursManuels = round(NbEpandeursManuels / NbEpandeurs * 100, 1)
  )
writeCSV(Partie6_epandageManuel)
