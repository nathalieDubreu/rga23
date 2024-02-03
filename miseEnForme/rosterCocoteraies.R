rga23_cocoteraies <- readTable("roster_coco_loc.tab", dossier) |>
  filter(!(interview__key %in% interviewKeyAExclure)) |>
  mutate(PartCoco = case_when(
    ToutRevenu == 1 ~ 100,
    TRUE ~ PartCoco
  )) |>
  select(!interview__id & !cocoGps__area & !cocoGps__len & !cocoGps__num & !cocoGps__racc & !cocoGps__rfrq) |>
  rename(cocoteraie_id = roster_coco_loc__id)

writeCSVTraites(rga23_cocoteraies)
