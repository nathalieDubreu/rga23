rga23_cocoteraies <- readTable("roster_coco_loc.tab", dossier) |>
  mutate(PartCoco = case_when(
    ToutRevenu == 1 ~ 100,
    TRUE ~ PartCoco
  )) |>
  select(!interview__id)

writeCSVTraites(rga23_cocoteraies)