# filter(id_exploitation %in% c("M10065", "P3514", "M10043", "P5480", "M10304"))

# filter(id_sia %in% c(
#   "7552",
#   "5444",
#   "5445",
#   "532",
#   "10934",
#   "10755",
#   "10928"
# ))

bio <- readCSV("rga23.csv") |>
  filter(AgriBio == 1 | AgriBio == 3) |>  
  mutate(`Bio ?` = case_when(
    AgriBio == 1 ~ "Oui en totalité",
    AgriBio == 3 ~ "Oui en partie"
  ))|>
  group_by(`Bio ?`) |>
  select(interview__key, id_exploitation, RaisonSociale, Nom, Prenoms, `Bio ?`, SurfaceTotalProdAgri) |>
  arrange(Nom, Prenoms)
writeCSV(bio)

base <- readCSV("BaseRGA_v9.csv") |>
  filter(id_sia %in% c(
    "4889",
    "8306",
    "5359",
    "3196",
    "7748",
    "7540",
    "3163",
    "9626",
    "10743"
  )) |>
  select(id_exploitation, id_sia)

allocatairesMoorea <- inner_join(rga23, base) |>
  select(interview__key, id_exploitation, id_sia, Ile, SurfaceTotalProdAgri, eligibilite, InstallationRecente, ArretActivite , AutoConsommation)

rga23_parcelles_locationPays <- left_join(readCSV("rga23_parcelles.csv"), readCSV("rga23_exploitations.csv") |> select(interview__key, IleExploitation)) |>
  filter(faireValoirParcelle == 2) |>
  group_by(IleExploitation) |>
  summarize(`Surface (en Ha)` = (sum(polygone__area, na.rm = TRUE) + sum(surfaceParcelleNonDelimitee, na.rm = TRUE)) / 10000)
# 1535 Hectares en tout

