## Variables diverses

rga23Brut <- readTable("rga23.tab", dossier) |>
  mutate(AbeillesBio = case_when(
    (AgriBio == 1 & PresenceAnimaux__7 == 1) ~ 1,
    (AgriBio == 2 & PresenceAnimaux__7 == 1) ~ 2,
    TRUE ~ AbeillesBio
  )) |>
  mutate(PartPlantsAutoP = case_when(
    (ProvenancePlants__2 == 0) ~ 1,
    (ProvenancePlants__2 == 1 & is.na(PartPlantsAutoP)) ~ 5,
    TRUE ~ PartPlantsAutoP
  )) |>
  mutate(PartSemencesAutoP = case_when(
    (ProvenanceSemences__2 == 0) ~ 1,
    (ProvenanceSemences__2 == 1 & is.na(PartSemencesAutoP)) ~ 5,
    TRUE ~ PartSemencesAutoP
  )) |>
  mutate(PartRevenusAgriExpl = case_when(
    (ActivitesChefExploit__1 == 1 & is.na(PartRevenusAgriExpl)) ~ 4,
    TRUE ~ PartRevenusAgriExpl
  )) |>
  mutate(PresSurfIrrigables = case_when(
    (Irrigation == 1) ~ 1,
    TRUE ~ PresSurfIrrigables
  )) |>
  mutate(SurfaceBioJardins = case_when(
    (AgriBio == 1 & !is.na(SurfaceJardins)) ~ 1,
    (AgriBio == 2 & !is.na(SurfaceJardins)) ~ 2,
    TRUE ~ SurfaceBioJardins
  )) |>
  mutate(SurfaceIrrigJardins = case_when(
    (Irrigation == 2 & !is.na(SurfaceJardins)) ~ 0,
    TRUE ~ SurfaceIrrigJardins
  ))

## Localisation

iles <- readCSV("iles.csv")
communes <- readCSV("communesISPF.csv")
ilesCommunesUniques <- readCSV("ilesCommunesUniques.csv")

### Ile d'exploitation

### Etape 1 : récupérer Ile d'exploitation et archipel à partir du code de l'île
rga23Localise1 <- left_join(rga23Brut |> rename(IleISPF = IleExploitation), iles, by = c("IleISPF")) |>
  rename(
    IleExploitationISPF = IleISPF,
    ArchipelExploitation = Subdivision,
    IleExploitation = Ile.y,
    Ile = Ile.x
  )

### Etape 2 : récupérer Commune d'exploitation à partir du code de la commune
rga23Localise2 <- left_join(rga23Localise1 |> rename(CommuneISPF = CommuneExploitation), communes, by = c("CommuneISPF")) |>
  rename(
    CommuneExploitationISPF = CommuneISPF,
    CommuneExploitation = CommuneAvecParentheses
  )

### Etape 3 : récupérer Commune unique d'exploitation à partir de l'île d'exploitation
rga23Localise3 <- left_join(rga23Localise2 |> rename(IleUnique = IleExploitation), ilesCommunesUniques, by = c("IleUnique")) |>
  mutate(CommuneExploitation = case_when(
    !is.na(CommuneUnique) ~ CommuneUnique,
    TRUE ~ CommuneExploitation
  )) |>
  rename(
    IleExploitation = IleUnique
  ) |>
  select(!CommuneUnique)

### Ile d'habitation

### Etape 1 : récupérer Ile d'habitation et archipel à partir du code de l'île
rga23Localise4 <- left_join(rga23Localise3 |> rename(IleISPF = Ile), iles, by = c("IleISPF")) |>
  rename(
    Archipel = Subdivision
  )

### Etape 2 : récupérer Commune d'habitation à partir du code de la commune
rga23Localise5 <- left_join(rga23Localise4 |> rename(CommuneISPF = Commune), communes, by = c("CommuneISPF")) |>
  rename(
    Commune = CommuneAvecParentheses
  )

### Etape 3 : récupérer Commune unique d'habitation à partir de l'île d'habitation
rga23Localise6 <- left_join(rga23Localise5 |> rename(IleUnique = Ile), ilesCommunesUniques, by = c("IleUnique")) |>
  mutate(Commune = case_when(
    !is.na(CommuneUnique) ~ CommuneUnique,
    TRUE ~ Commune
  )) |>
  rename(
    Ile = IleUnique
  ) |>
  select(!CommuneUnique)

rga23 <- rga23Localise6

rm(
  rga23Brut,
  rga23Localise1,
  rga23Localise2,
  rga23Localise3,
  rga23Localise4,
  rga23Localise5,
  rga23Localise6
)
