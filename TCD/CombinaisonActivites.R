combinaisonsActivites <- left_join(
  rga23_champ_Ile_Commune,
  readCSV("rga23_tousFichiersPlats.csv") |> select(
    interview__key,
    starts_with("PresenceAnimaux__"),
    starts_with("CulturesPresentes__"),
    SurfaceJardins,
    ActivitesChefExploit__5
  ),
  by = "interview__key"
) |> mutate(
  # Cultures maraîchères....................................10/10
  activiteA = ifelse(replace_na(CulturesPresentes__10, 0) == 1, " Maraichage /", "-/"),
  # Cultures vivrières......................................20 /20
  activiteB = ifelse(replace_na(CulturesPresentes__20, 0) == 1, " Vivrier /", "-/"),
  # Cultures fruitières (hors pépinères) et bois d'oeuvre...30 /30
  activiteC = ifelse(replace_na(CulturesPresentes__30, 0) == 1, " Fruitier /", "-/"),
  # Feuillages et cultures florales (hors pépinières).......40 /40
  activiteD = ifelse(replace_na(CulturesPresentes__40, 0) == 1, " Floral /", "-/"),
  # Plantes aromatiques, stimulantes et médicinales.........50 /50
  activiteE = ifelse(replace_na(CulturesPresentes__50, 0) == 1, " Ppam /", "-/"),
  # Pépinières (plantes vendues en pot).....................60 /60
  activiteF = ifelse(replace_na(CulturesPresentes__60, 0) == 1, " Pepinieres /", "-/"),
  # Cultures fourragères....................................70 /70
  activiteG = ifelse(replace_na(CulturesPresentes__70, 0) == 1, " Fourrageres /", "-/"),
  # Jachères................................................80 /80
  activiteH = ifelse(replace_na(CulturesPresentes__80, 0) == 1, " Jacheres /", "-/"),
  # Jardins océaniens
  activiteI = ifelse(replace_na(SurfaceJardins, 0) > 0, " JardinsOceaniens /", "-/"),
  # Bovins..................................................1 /1
  activiteJ = ifelse(replace_na(PresenceAnimaux__1, 0) == 1, " Bovins /", "-/"),
  # Ovins...................................................2 /2
  activiteK = ifelse(replace_na(PresenceAnimaux__2, 0) == 1, " Ovins /", "-/"),
  # Porcins.................................................3 /3
  activiteL = ifelse(replace_na(PresenceAnimaux__3, 0) == 1, " Porcins /", "-/"),
  # Volailles...............................................4 /4
  activiteM = ifelse(replace_na(PresenceAnimaux__4, 0) == 1, " Volailles /", "-/"),
  # Equidés.................................................5 /5
  activiteN = ifelse(replace_na(PresenceAnimaux__5, 0) == 1, " Equides /", "-/"),
  # Lapins élevés pour la chair.............................6 /6
  activiteO = ifelse(replace_na(PresenceAnimaux__6, 0) == 1, " Lapins /", "-/"),
  # Abeilles................................................7 /7
  activiteP = ifelse(replace_na(PresenceAnimaux__7, 0) == 1, " Abeilles /", "-/"),
  # Caprins.................................................8 /8
  activiteQ = ifelse(replace_na(PresenceAnimaux__8, 0) == 1, " Caprins /", "-/"),
  # Peche
  activiteR = ifelse(replace_na(ActivitesChefExploit__5, 0) == 1, " Peche /", "-/"),
  # Coprah
  activiteS = ifelse(replace_na(ProducteursCoprah, 0) == 1, " Coprah", "-"),
  # Concaténation 
  Activites = paste0(
    activiteA, activiteB, activiteC, activiteD, activiteE, activiteF, 
    activiteG, activiteH, activiteI, activiteJ, activiteK, activiteL, 
    activiteM, activiteN, activiteO, activiteP, activiteQ, activiteR, activiteS
  )
)

comptagesCombinaisonsActivites <- combinaisonsActivites |> 
  group_by(Activites) |> 
  summarize(`Nombre d'exploitants` = n()) |> 
  arrange(desc(`Nombre d'exploitants`))

writeCSV(comptagesCombinaisonsActivites)
