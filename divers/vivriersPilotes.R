# Iles
## Huahine, Tahaa, Rimatara, Rurutu, Ua Huka, Bora Bora, Nuku Hiva
ilesConcernees <- c(31, 88, 84, 86, 114, 14, 67)
# Communes
## Taputapuatea (Raiatea) - 3 communes associées, Teva i Uta - 2 communes associées - et Mahina (Tahiti)
communesConcernees <- c(501, 502, 503, 521, 522, 250)

rga23VivriersPilotes <- readCSV("rga23.csv") |>
  filter(CulturesPresentes__20 == 1 | CultPresentesJardins__20 == 1) |>
  filter(IleISPF %in% ilesConcernees |
    IleExploitationISPF %in% ilesConcernees |
    CommuneISPF %in% communesConcernees |
    CommuneExploitationISPF %in% communesConcernees)
# 465 -> 466 -> 684 -> 694

ids <- rga23VivriersPilotes |> select(interview__key)

surfacesVivriersPilotes <- inner_join(readCSV("rga23_surfacesCultures.csv"), ids) |>
  filter(TypeCulture == 20)

generalPilotes <- inner_join(readCSV("rga23_general.csv"), ids) |>
  select_if(~ n_distinct(.) > 1)
exploitationsPilotes <- inner_join(readCSV("rga23_exploitations.csv"), ids) |>
  select_if(~ n_distinct(.) > 1)
cultivateursPilotes <- inner_join(readCSV("rga23_prodVegetales.csv"), ids) |>
  select_if(~ n_distinct(.) > 1)

repartitionPilote <- rga23VivriersPilotes |> mutate(LieuExploitation = case_when(CommuneExploitationISPF %in% c(521,522) ~ "Teva i Uta",
                                                                    IleExploitationISPF == 90 ~ CommuneExploitation,
                                                                    CommuneExploitationISPF %in% c(501, 502, 503) ~ "Taputapuatea",
                                                            TRUE ~ IleExploitation),
                               LieuHabitation = case_when(CommuneISPF %in% c(521,522) ~ "Teva i Uta",
                                                          IleISPF == 90 ~ Commune,
                                                          CommuneISPF %in% c(501, 502, 503) ~ "Taputapuatea",
                                                          TRUE ~ Ile)) |> group_by(LieuExploitation, LieuHabitation) |> count()

writeCSV(repartitionPilote)

# test <- rga23VivriersPilotes |> filter(totalSurfaceVivri >= 500 | SurfaceJardins >= 500)  |> select(totalSurfaceVivri, SurfaceJardins)
# test <- rga23VivriersPilotes |> filter(is.na(totalSurfaceVivri)) |> select(interview__key, SurfaceJardins)

rga23VivriersPilotes <- rga23VivriersPilotes |> select(!SituationConjChefExpl)
writeCSV(rga23VivriersPilotes)
writeCSV(surfacesVivriersPilotes)
