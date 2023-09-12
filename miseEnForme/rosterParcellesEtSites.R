rga23_sites <- readTable("roster_accesSite", dossier) 
rga23_parcelles <- readTable("roster_parcelles", dossier) 

writeCSVTraites(rga23_sites)
writeCSVTraites(rga23_parcelles)