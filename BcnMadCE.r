if(Sys.info()["sysname"] == "Linux"){
  data_add <- paste0("/media/",
                     system("whoami", intern = TRUE),
                     "/",
                     system(paste0("ls /media/",system("whoami", intern = TRUE)), intern = TRUE),
                     "/PSSJD/RESPOND/data/source/")
  data_add <- data_add[file.exists(data_add)]
} else if(Sys.info()["sysname"] == "Windows"){
  data_add <- paste0(grep("^[A-Z]:$", sub(":(.*)", ":",shell("wmic logicaldisk get name", intern = TRUE)), value = TRUE), "/PSSJD/RESPOND/data/source")
  data_add <- data_add[file.exists(data_add)]
  data_add <- paste0(data_add, "/")
}

library(data.table)
T1 <- fread(paste0(data_add, "BcnMadCE/RESPOND WP4 T1_V2_enviada_CSV.csv"))
T2 <- fread(paste0(data_add, "BcnMadCE/RESPOND WP4 T2_V2_enviada_CSV.csv"))
T3 <- fread(paste0(data_add, "BcnMadCE/RESPOND WP4 T3_V2_enviada_CSV.csv"))
T4 <- fread(paste0(data_add, "BcnMadCE/RESPOND WP4 T4_V2_enviada_CSV.csv"))
