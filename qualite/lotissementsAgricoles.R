library("stringr")
fichierLotissements <- readCSV("lotissements/attributaires.csv") |> filter(`Civ.` != "")

head(fichierLotissements)

fichierHorsDoublons1 <- fichierLotissements |>
  rename(Nom = NOM, Prenoms = `Prénom(s)`) |>
  mutate(Superficie = str_replace(`Superficie (m²)`, ",", "")) |>
  group_by(ARCHIPEL, Iles, Lotissement, Nom, Prenoms, `Vini / tél. fixe`, VILLE, Commune) |>
  summarise(
    SurfaceLouee = sum(as.numeric(Superficie), na.rm = TRUE),
    NombreDeLots = n(),
    NumerosDesLots = as.character(list(`N° Lot`))
  )

fichierHorsDoublons2 <- fichierHorsDoublons1 |>
  group_by(ARCHIPEL, Iles, , VILLE, Nom, Prenoms) |>
  summarise(
    Telephones = as.character(list(`Vini / tél. fixe`)),
    SurfaceLouee = sum(SurfaceLouee, na.rm = TRUE),
    NombreDeLots = sum(NombreDeLots),
    NumerosDesLots = as.character(list(NumerosDesLots)),
    Lotissements = as.character(list(Lotissement)),
    Communes = as.character(list(Commune))
  )

writeCSVTraites(fichierHorsDoublons2)
