## Récupération de la dernière version de la base

baseRGA <- readCSV("BaseRGA_v9.csv")

baseRGA |>
  group_by(substring(id_exploitation, 0, 1)) |>
  count()

### Récupération rga23 "brut" pour déterminer les identifiants en X à modifier
rga23 <- readTable("rga23.tab", dossier)

# Passage des identifiants X

## En P s'ils ne font plus de coprah
xAPasserEnP <- rga23 |>
  filter(substring(id_exploitation, 0, 1) == "X") |>
  filter(eligibilite == 1 & eligibiliteCoprah == 0) |>
  mutate(id_exploitation_modif = gsub("X", "P", id_exploitation)) |>
  select(id_exploitation, id_exploitation_modif)
### 4 cas

baseRGA <- left_join(baseRGA, xAPasserEnP, by=c("id_exploitation")) |>
  mutate(id_exploitation = case_when(
    !is.na(id_exploitation_modif) ~ id_exploitation_modif,
    TRUE ~ id_exploitation
  )) |>
  select(!id_exploitation_modif)

# En C s'ils ne font que du coprah
xAPasserEnC <- rga23 |>
  filter(substring(id_exploitation, 0, 1) == "X") |>
  filter(eligibilite == 0 & eligibiliteCoprah == 1) |>
  mutate(nouvelId = 1750 + row_number(), id_exploitation_modif = paste("C", nouvelId, sep = "")) |>
  select(id_exploitation, id_exploitation_modif)
## 29 cas

baseRGA <- left_join(baseRGA, xAPasserEnC, by=c("id_exploitation")) |>
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

baseRGA <- left_join(baseRGA, doublonsAPasserEnX, by=c("id_exploitation")) |>
  mutate(id_exploitation = case_when(
    !is.na(id_exploitation_modif) ~ id_exploitation_modif,
    TRUE ~ id_exploitation
  )) |>
  select(!id_exploitation_modif)


# baseRGA |>
#   group_by(substring(id_exploitation, 0, 1)) |>
#   count()

rm(xAPasserEnC, xAPasserEnP, doublonsAPasserEnX, rga23)

BaseRGA_def <- baseRGA

writeCSVTraites(BaseRGA_def)

