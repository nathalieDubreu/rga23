# Comparaison données du BSE - 29/04/2024

## Champ 1 : présence de cultures maraîchères (mode de production classique) + indicRGA23 == 1

rga23_champ <- inner_join(readCSV("rga23_prodVegetales.csv") |> filter(CulturesPresentes__10 == 1) |> select(interview__key),
  readCSV("rga23_general.csv") |> filter(indicRGA23 == 1) |> select(interview__key),
  by = "interview__key"
) |> left_join(readCSV("rga23_exploitations.csv") |> select(interview__key, ArchipelExploitation, IleExploitation))

rga23_surfacesCulturesMarai <- inner_join(
  readCSV("rga23_surfacesCultures.csv") |> filter(TypeCulture == 10),
  rga23_champ,
  by = "interview__key"
)

## Surface par culture et par île
surfacesDetailleesParCultMarai_Ile <- rga23_surfacesCulturesMarai |>
  group_by(culture_id, IleExploitation) |>
  summarize(
    surface = sum(SurfaceCult, na.rm = TRUE),
    nombre = n_distinct(interview__key)
  ) |>
  left_join(readInputCSV("cultures.csv"))

## Incohérence - test sur Raiatea
inner_join(
  readCSV("rga23_surfacesCultures.csv"),
  rga23_champ,
  by = "interview__key"
) |>
  left_join(readInputCSV("cultures.csv")) |>
  filter(IleExploitation == "Raiatea" & libelleCulture == "Autres") |>
  group_by(culture_id, IleExploitation) |>
  summarize(
    surface = sum(SurfaceCult, na.rm = TRUE),
    nombre = n_distinct(interview__key)
  ) |>
  left_join(readInputCSV("cultures.csv"))

## Surface maraichage totale de l'île
surfacesCultMarai_Ile <- rga23_surfacesCulturesMarai |>
  group_by(IleExploitation) |>
  summarize(
    surface = sum(SurfaceCult, na.rm = TRUE),
    nombre = n_distinct(interview__key)
  )

## Surface par culture et par archipel
surfacesDetailleesParCultMarai_Archipel <- rga23_surfacesCulturesMarai |>
  group_by(culture_id, ArchipelExploitation) |>
  summarize(
    surface = sum(SurfaceCult, na.rm = TRUE),
    nombre = n_distinct(interview__key)
  ) |>
  left_join(readInputCSV("cultures.csv"))

## Surface maraichage totale de l'archipel
surfacesCultMarai_Archipel <- rga23_surfacesCulturesMarai |>
  group_by(ArchipelExploitation) |>
  summarize(
    surface = sum(SurfaceCult, na.rm = TRUE),
    nombre = n_distinct(interview__key)
  )

## Champ 2 : surfaces des cocotiers + indicRGA23 == 1

rga23_champ <- inner_join(
  readCSV("rga23_general.csv") |> filter(indicRGA23 == 1) |> select(interview__key),
  readCSV("rga23_exploitations.csv") |> select(interview__key, ArchipelExploitation, IleExploitation),
  by = "interview__key"
)

rga23_surfacesCulturesCoco <- inner_join(
  readCSV("rga23_surfacesCultures.csv") |> filter(culture_id == 307 | culture_id == 308 | culture_id == 309),
  rga23_champ,
  by = "interview__key"
)

surfacesDetailleesCultCoco_Ile <- rga23_surfacesCulturesCoco |>
  group_by(culture_id, IleExploitation) |>
  summarize(
    surface = sum(SurfaceCult, na.rm = TRUE),
    nombre = n_distinct(interview__key)
  )

surfacesCultCoco_Ile <- rga23_surfacesCulturesCoco |>
  group_by(IleExploitation) |>
  summarize(
    surface = sum(SurfaceCult, na.rm = TRUE),
    nombre = n_distinct(interview__key)
  )
