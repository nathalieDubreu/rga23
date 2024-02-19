## Modifications des identifiants

# rga23 |>
#   group_by(substring(id_exploitation, 0, 1)) |>
#   count()

# Passage des identifiants X

## En P s'ils ne font plus de coprah
# xAPasserEnP <- rga23 |>
#   filter(substring(id_exploitation, 0, 1) == "X") |>
#   filter(eligibilite == 1 & eligibiliteCoprah == 0)
### 4 cas

rga23 <- rga23 |> mutate(id_exploitation = case_when(
  (substring(id_exploitation, 0, 1) == "X" & eligibilite == 1 & eligibiliteCoprah == 0) ~ gsub("X", "P", id_exploitation),
  TRUE ~ id_exploitation
))

# En C s'ils ne font que du coprah
xAPasserEnC <- rga23 |>
  filter(substring(id_exploitation, 0, 1) == "X") |>
  filter(eligibilite == 0 & eligibiliteCoprah == 1) |>
  arrange(interview__key) |>
  mutate(nouvelId = 1750 + row_number(), id_exploitation_modif = paste("C", nouvelId, sep = "")) |>
  select(id_exploitation, id_exploitation_modif)
## 29 cas

rga23 <- left_join(rga23, xAPasserEnC, by=c("id_exploitation")) |>
  mutate(id_exploitation = case_when(
    !is.na(id_exploitation_modif) ~ id_exploitation_modif,
    TRUE ~ id_exploitation
  )) |>
  select(!id_exploitation_modif)

# Passage des identifiants P en X en cas de doublons avec un C

doublonsAPasserEnX <- readInputCSV("doublons_v5.csv") |>
  filter(Valide == "OK") |>
  rename(id_exploitation = idExploit) |>
  mutate(id_exploitation_modif = gsub("P", "X", id_exploitation)) |>
  select(id_exploitation, id_exploitation_modif)
## 25 cas

rga23 <- left_join(rga23, doublonsAPasserEnX, by=c("id_exploitation")) |>
  mutate(id_exploitation = case_when(
    !is.na(id_exploitation_modif) ~ id_exploitation_modif,
    TRUE ~ id_exploitation
  )) |>
  select(!id_exploitation_modif)


# rga23 |>
#   group_by(substring(id_exploitation, 0, 1)) |>
#   count()

rm(xAPasserEnC, xAPasserEnP, doublonsAPasserEnX)

writeCSVTraites(rga23)


## Ajout de l'interview__id dans la base d interrogation - Suppression des variables inutiles
# baseComplete <- readCSV("BaseRGA_def.csv")
# baseBis <- inner_join(rga23 |>
#                         select(interview__key, id_exploitation), baseComplete, by = c("id_exploitation")) |>
#   select(!Statut &
#            !DateNaissChefExpl &
#            !Commentaire &
#            !ile_hab_codeISPF &
#            !ile_hab &
#            !commune_hab_codeISPF &
#            !commune_hab &
#            !adresse_hab &
#            !ile_expl_codeISPF &
#            !ile_expl &
#            !commune_expl_codeISPF &
#            !commune_expl &
#            !adresse_expl &
#            !derniereAnneeCoprah &
#            !numEnqueteur &
#            !geo_ile &
#            !geo_com) |>
#   mutate_all(~ifelse(is.na(.), "", .))
