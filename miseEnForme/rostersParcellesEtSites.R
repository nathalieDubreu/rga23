rga23_sites <- readTable("roster_accesSite.tab", dossier) 
rga23_parcelles <- readTable("roster_parcelles.tab", dossier) 

writeCSVTraites(rga23_sites)
writeCSVTraites(rga23_parcelles)
