
rga23_coexploitants <- readTable("RosterCoExploit", dossier) 
rga23_moPermanenteNonFam <- readTable("MONonFamPerm.tab", dossier) 
rga23_moPermanenteFam <- readTable("RosterMOPermFam", dossier) 

writeCSVTraites(rga23_coexploitants)
writeCSVTraites(rga23_moPermanenteNonFam)
writeCSVTraites(rga23_moPermanenteFam)
