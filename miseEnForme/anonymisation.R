rga23_coprahculteurs <- rga23_coprahculteurs |>
  select(
    -debutCoprahculture,
    -finCoprahculture
  )

rga23_exploitations <- rga23_exploitations |>
  select(
    -debutLocalisation,
    -gpsExploitation__Timestamp,
    -AdressePhysiqueExploitation,
    -finLocalisation
  )

rga23_general <- rga23_general |>
  select(
    -AncienNom,
    -AnciensPrenoms,
    -AnciensTelephones,
    -Nom,
    -Prenoms,
    -Telephone,
    -Surnom,
    -Email,
    -AdressePhysique
  )

rga23_prodAnimales <- rga23_prodAnimales |>
  select(
    -debutProdAnimales,
    -finProdAnimales
  )

rga23_prodVegetales <- rga23_prodVegetales |>
  select(
    -debutProdVegetales,
    -finProdVegetales
  )

rga23_mainOeuvre <- rga23_mainOeuvre |>
  select(
    -debutMainOeuvre,
    -finMainOeuvre
  )

rga23_gestion <- rga23_gestion |>
  select(
    -interview__id,
    -id_enqueteur_ech,
    -enqueteur,
    -autre_enqueteur,
    -sssys_irnd,
    -has__errors,
    -interview__status,
    -assignment__id
  )

rga23_parcelles <- rga23_parcelles |>
  select(
    -adresseSurfaceNonDelimitee,
    -gps__Accuracy,
    -gps__Altitude,
    -gps__Latitude,
    -gps__Longitude,
    -gps__Timestamp
  )

## Regroupement du rga23_general et du rga23_gestion
rga23_general <- left_join(rga23_general, rga23_gestion)

writeCSVTraites(rga23_coprahculteurs)
writeCSVTraites(rga23_exploitations)
writeCSVTraites(rga23_general)
writeCSVTraites(rga23_prodAnimales)
writeCSVTraites(rga23_prodVegetales)
writeCSVTraites(rga23_mainOeuvre)
writeCSVTraites(rga23_gestion)
writeCSVTraites(rga23_parcelles)
