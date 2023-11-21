library("stringr")
attributaires <- rbind(
  readCSV("AttributairesTraites/lotissementsAustrales.csv") |> rename(Lotissements = Lotissement),
  readCSV("AttributairesTraites/lotissementsMarquises.csv") |> rename(Lotissements = Lotissement),
  readCSV("AttributairesTraites/lotissementsHuahineTahaa.csv") |> mutate(VILLE = "", Commune = ""),
  readCSV("AttributairesTraites/lotissementsMoorea.csv"),
  readCSV("AttributairesTraites/lotissementsRaiatea_1.csv") |> mutate(VILLE = "", Commune = ""),
  readCSV("AttributairesTraites/lotissementsRaiatea_2.csv") |> mutate(VILLE = "", Commune = ""),
  readCSV("AttributairesTraites/lotissementsTahiti.csv")
)

#########################################
# Attributaires présents dans la base   #
#########################################

attributairesPres <- attributaires |>
  filter(!is.na(NumUnite) & !is.na(Telephones)) |>
  rename(id_exploitation = NumUnite)

rga <- readCSV("rga23.csv")

injoignables <- inner_join(attributairesPres,
  rga |> filter(statut_collecte == 2 | statut_collecte == 3),
  by = c("id_exploitation")
) |>
  mutate(AVerifier = "Classé en injoignable ou n'existe plus alors qu'ils louent des terres domaniales") |>
  select(interview__key, id_exploitation, id_enqueteur_ech, SurfaceLouee, AVerifier, Telephones, tel_ech)

refus <- inner_join(attributairesPres,
  rga |> filter(statut_collecte == 4),
  by = c("id_exploitation")
) |>
  mutate(AVerifier = "Refus de répondre alors qu'ils louent des terres domaniales") |>
  select(interview__key, id_exploitation, id_enqueteur_ech, SurfaceLouee, AVerifier, Telephones, tel_ech)


## Répondants non éligibles hors installations récentes
repondantsNonEligibles <- inner_join(attributairesPres,
  rga |> filter(eligibilite == 0 & is.na(InstallationRecente)),
  by = c("id_exploitation")
) |>
  filter(id_exploitation != "M10282" & id_exploitation != "M10219") |>
  mutate(AVerifier = case_when(
    AutoConsommation == 1 ~ "Consomme tout ce qu'il produit notamment sur la location de terre domaniale ???",
    as.numeric(substring(DateCessationActivite, 0, 4)) <= 2022 ~ "Fin d'activité avant 2022 mais il loue encore ???",
    TRUE ~ "??"
  )) |>
  select(interview__key, id_exploitation, id_enqueteur_ech, SurfaceLouee, AVerifier, Telephones, tel_ech)

aVerifier <- rbind(injoignables, refus, repondantsNonEligibles) |> rename(TelephoneDagLocationTerres = Telephones)
writeCSV(aVerifier)

baseRGA <- readCSV("AttributairesTraites/carnetsTournee.csv")
nonRepondantsYet <- inner_join(anti_join(attributairesPres, rga, by = c("id_exploitation")), baseRGA, by = c("id_exploitation")) |>
  mutate(
    Telephones = str_replace_all(Telephones, " ", ""),
    tel_ech = str_replace_all(tel_ech, " ", "")
  ) |>
  select(Iles, id_enqueteur_ech, id_exploitation, Telephones, tel_ech) |>
  filter(Telephones != tel_ech)

# writeCSV(nonRepondantsYet)
# readCSV("AttributairesTraites/NumeroTelCompl.csv") |> group_by(Ile) |> count()
# readCSV("AttributairesTraites/NumeroTelCompl.csv") |> filter(Ile == "TAHITI") |> group_by(id_enqueteur_ech) |> count()

#########################################
# Attributaires absents de la base      #
#########################################

attributairesAbs <- attributaires |>
  filter(Absent == 1) |>
  select(!NumUnite) |>
  arrange(Iles,Nom,Prenoms)

attributairesAbs |>
  group_by(Iles) |>
  count()

writeCSV(attributairesAbs)

