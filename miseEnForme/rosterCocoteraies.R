rga23_cocoteraies <- readTable("roster_coco_loc.tab", dossier) |>
  filter(interview__key != "59-36-31-34" &
    interview__key != "06-79-34-97" &
    interview__key != "26-72-53-00" &
    interview__key != "49-29-35-86" &
    interview__key != "93-83-94-94") |>
  mutate(PartCoco = case_when(
    ToutRevenu == 1 ~ 100,
    TRUE ~ PartCoco
  )) |>
  select(!interview__id & !cocoGps__area & !cocoGps__len & !cocoGps__num & !cocoGps__racc & !cocoGps__rfrq) |>
  rename(cocoteraie_id = roster_coco_loc__id)

writeCSVTraites(rga23_cocoteraies)
