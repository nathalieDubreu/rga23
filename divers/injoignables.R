rga23 <- readCSV("rga23.csv")

idInjoignables <- rga23 |>
  filter(statut_collecte == 3) |>
  select(
    interview__key,
    id_exploitation,
  )

base <- readCSV("BaseRGA_v9.csv")

injoignables <- left_join(idInjoignables, base)

writeCSV(injoignables)


# interview__key,
# id_exploitation,
# nom_ech, 
# prenoms_ech, 
# tel_ech, 
# IleISPF, 
# CommuneISPF, 
# AdressePhysique, 
# IleExploitationISPF, 
# CommuneExploitationISPF, 
# AdressePhysiqueExploitation