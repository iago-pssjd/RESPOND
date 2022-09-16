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
library(matrixStats)
library(ggplot2)

screening <- readLines(paste0(data_add, "BcnMadCE/BCN_Spain_export_20220906_v2.csv"))
# table(sapply(gregexpr(";", screening), length))
# table(sapply(gregexpr(",", screening), length))
# gregexpr(",", screening) |> unlist() |> table()
# screening <- gsub(",", ";", screening)
screening <- fread(text = screening, sep = ";", na.strings = c("NA", ""), key = "Record Id")
setnames(screening, \(.x) gsub("\\s", "_", .x))

T1 <- readLines(paste0(data_add, "BcnMadCE/RESPOND WP4 T1_V2_enviada_CSV.csv"))
# table(sapply(gregexpr(";", T1), length))
# table(sapply(gregexpr(",", T1), length))
# gregexpr(",", T1) |> unlist() |> table()
# T1 <- gsub(",", ";", T1)
T1 <- fread(text = T1, sep = ";", na.strings = c("NA", ""), key = "Castor Record ID")
setnames(T1, \(.x) gsub("\\s", "_", .x))
setnames(T1, "Survey_Completed_On", "t1_Survey_Completed_On")

T2 <- readLines(paste0(data_add, "BcnMadCE/RESPOND WP4 T2_V2_enviada_CSV.csv"))
# table(sapply(gregexpr(";", T2), length))
# table(sapply(gregexpr(",", T2), length))
# gregexpr(",", T2) |> unlist() |> table()
T2 <- gsub(",", ".", T2)
T2 <- fread(text = T2, sep = ";", na.strings = c("NA", ""), key = "Castor Record ID")
setnames(T2, \(.x) gsub("\\s", "_", .x))
setnames(T2, "Survey_Completed_On", "t2_Survey_Completed_On")

T3 <- readLines(paste0(data_add, "BcnMadCE/RESPOND WP4 T3_V2_enviada_CSV.csv"))
# table(sapply(gregexpr(";", T3), length))
# table(sapply(gregexpr(",", T3), length))
# gregexpr(",", T3) |> unlist() |> table()
T3 <- gsub(",", ".", T3)
T3 <- fread(text = T3, sep = ";", na.strings = c("NA", ""), key = "Castor Record ID")
setnames(T3, \(.x) gsub("\\s", "_", .x))
setnames(T3, "Survey_Completed_On", "t3_Survey_Completed_On")

T4 <- readLines(paste0(data_add, "BcnMadCE/RESPOND WP4 T4_V2_enviada_CSV.csv"))
# table(sapply(gregexpr(";", T4), length))
# table(sapply(gregexpr(",", T4), length))
# gregexpr(",", T4) |> unlist() |> table()
# T4 <- gsub(",", ";", T4)
T4 <- fread(text = T4, sep = ";", na.strings = c("NA", ""), key = "Castor Record ID")
setnames(T4, \(.x) gsub("\\s", "_", .x))
setnames(T4, "Survey_Completed_On", "t4_Survey_Completed_On")

# previous arrangements to melting data
T1[, `:=` (t1_csri_sp_off_heath_t = as.numeric(t1_csri_sp_off_heath_t))]
T4[, `:=` (t4_m_T1_CSRI_SP_nurse_g_t = as.integer(t4_m_T1_CSRI_SP_nurse_g_t), t4_csri_sp_off_heath_t = as.numeric(t4_csri_sp_off_heath_t))]


T12 <- merge(T1, T2, all = TRUE)
T34 <- merge(T3, T4, all = TRUE)
Tdata <- merge(T12, T34, all = TRUE)



Tlong <- melt(Tdata, 
            measure.vars = measure(wave = as.integer, value.name, pattern = "^t([1234])_(.*)"))
Tlong <- na.omit(Tlong, cols = "Survey_Completed_On")


eTlong <- merge(screening, Tlong, all = TRUE, by.x = "Record_Id", by.y = "Castor_Record_ID")
eTlong[, phq9 := rowSums2(as.matrix(.SD)), .SDcols = patterns("^phq9_0\\d")
       ][, gad7 := rowSums2(as.matrix(.SD)), .SDcols = patterns("^gad7_\\d")
         ][, phq_ads := phq9 + gad7]

eTlong[, lapply(.SD, mean, na.rm = TRUE), .SDcols = c("phq_ads"), by = .(wave, Randomization_Group)] |> 
  ggplot(aes(x = factor(wave), y = phq_ads, colour = Randomization_Group)) +
  geom_line()

na.omit(eTlong, cols = "phq_ads") |> 
  ggplot(aes(x = factor(wave), y = phq_ads, colour = Randomization_Group)) +
  stat_summary(aes(group = Randomization_Group), geom = "line", fun = mean) +
  stat_boxplot(geom = "errorbar", position = "identity")
