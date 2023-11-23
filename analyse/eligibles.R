rga23 <- readCSV("rga23.csv")

eligiblesRGA <- rga23 |>
  filter((interview__status == 100 | interview__status == 120 | interview__status == 130) & statut_collecte == 1) |>
  filter((eligibiliteCoprah == 1 & substring(id_exploitation, 0, 1) == "C") |
    (eligibilite == 1 & (substring(id_exploitation, 0, 1) == "P" | substring(id_exploitation, 0, 1) == "M")) |
    ((eligibilite == 1 | eligibiliteCoprah == 1) & substring(id_exploitation, 0, 1) == "X"))

eligiblesRGA |>
  filter(substring(id_exploitation, 0, 1) == "X" &
    eligibilite == FALSE & eligibiliteCoprah == TRUE) |>
  count()

# !!! BUG de non ouverture de certains X !!!
## Ajouter ceux qu'on ne peut pas "sauver"...
eligiblesRGA |>
  filter(substring(id_exploitation, 0, 1) == "X" &
    eligibilite == FALSE & eligibiliteCoprah == TRUE &
    interview__key != "05-81-20-62" &
    interview__key != "91-37-53-21" &
    interview__key != "38-13-87-05" &
    interview__key != "87-50-31-74" &
    interview__key != "88-51-69-94" &
    interview__key != "26-04-00-30" &
    interview__key != "39-17-18-65" &
    interview__key != "92-89-85-76" &
    interview__key != "80-90-88-92" &
    interview__key != "74-71-45-49" &
    interview__key != "85-03-43-60" &
    interview__key != "15-49-55-56" &
    interview__key != "80-20-52-20" &
    interview__key != "42-24-21-22" &
    interview__key != "29-46-58-12" &
    interview__key != "70-24-84-72" &
    interview__key != "51-37-96-71" &
    interview__key != "15-34-36-43" &
    interview__key != "46-51-80-33" &
    interview__key != "73-82-87-44" &
    interview__key != "13-18-12-26" &
    interview__key != "92-27-93-63" &
    interview__key != "37-25-09-12" &
    interview__key != "68-62-34-63" &
    interview__key != "88-47-99-60" &
    interview__key != "48-67-04-30")

# Main d'oeuvre familiale = OUI puis nombre de personnes = 0
aVerifier <- eligiblesRGA |>
  filter(MOPermanenteFamiliale == 1 & NbMOPermFamiliale == 0) |>
  select(interview__key, interview__status, id_enqueteur_ech)

# Pourcentage de destination des produits erroné
eligiblesRGA |> filter((totalCommMaraich != 0 & totalCommMaraich != 100) |
  (totalCommVivri != 0 & totalCommVivri != 100) |
  (totalCommPepinieres != 0 & totalCommPepinieres != 100) |
  (totalCommFourrages != 0 & totalCommFourrages != 100) |
  (totalCommFruit != 0 & totalCommFruit != 100) |
  (totalCommFlorale != 0 & totalCommFlorale != 100) |
  (totalCommPlantes != 0 & totalCommPlantes != 100) |
  (totalCommViande != 0 & totalCommViande != 100) |
  (totalCommOeufs != 0 & totalCommOeufs != 100) |
  (totalCommMiel != 0 & totalCommMiel != 100))

# Conjoint compté en tant que co-exploitant ET main d'oeuvre perm familiale
conjointsCoExpl <- readCSV("rga23_coexploitants.csv") |> filter(LienCoExploit == 1)
conjointsMOPerm <- readCSV("rga23_moPermanenteFam.csv") |> filter(LienMOFamPerm == 1)
conjointsARejetter <- left_join(inner_join(conjointsCoExpl, conjointsMOPerm, by = c("interview__key")), rga23 |> select(interview__key, id_enqueteur_ech, NbCoExploitants, NbMOPermFamiliale), by = c("interview__key")) |>
  select(interview__key, interview__status, id_enqueteur_ech, NbCoExploitants, NbMOPermFamiliale)

# Main d'oeuvre occasionnelle
aVerifier <- rga23 |>
  mutate(totalMAOccas = ifelse(is.na(NbFemOccasAvecLien), 0, NbFemOccasAvecLien) +
    ifelse(is.na(NbFemOccasSansLien), 0, NbFemOccasSansLien) +
    ifelse(is.na(NbHomOccasAvecLien), 0, NbHomOccasAvecLien) +
    ifelse(is.na(NbHomOccasSansLien), 0, NbHomOccasSansLien)) |>
  filter(totalMAOccas > 7 & (UniteDureeMOOccas == 1 | UniteDureeMOOccas == 2) & (DureeMOOccas < totalMAOccas | DureeMOOccas < 10)) |>
  select(interview__key, interview__status, id_enqueteur_ech, totalMAOccas, UniteDureeMOOccas, DureeMOOccas)
