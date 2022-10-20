# BcnMadCEdata_wran.r
# from BcnMadCEdata_arr.r
# to BcnMadCEdata_SR.r



# OS ddependencies ----------------------------------------------------------


if(Sys.info()["sysname"] == "Linux"){
	data_path <- paste0("/media/",
			   system("whoami", intern = TRUE),
			   "/",
			   system(paste0("ls /media/",system("whoami", intern = TRUE)), intern = TRUE),
			   "/PSSJD/RESPOND/data/source/")
	data_path <- data_path[file.exists(data_path)]
	dev_lib_path <- "~/R/x86_64-pc-linux-gnu-library/dev"
} else if(Sys.info()["sysname"] == "Windows"){
	data_path <- paste0(grep("^[A-Z]:$", sub(":(.*)", ":",shell("wmic logicaldisk get name", intern = TRUE)), value = TRUE), "/PSSJD/RESPOND/data/source")
	data_path <- data_path[file.exists(data_path)]
	data_path <- paste0(data_path, "/")
	dev_lib_path <- .libPaths()
}


# Libraries ---------------------------------------------------------------



library(matrixStats)
library(data.table, lib.loc = dev_lib_path)






# Data loading --------------------------------------------------------------

screening <- fread(paste0(data_path, "BcnMadCE/BcnData/BCN_Spain_export_20220906_v2.csv"), sep = ";", na.strings = c("NA", ""), key = "Record Id")
T1 <- fread(paste0(data_path, "../target/BcnMadCE/BcnData/RESPOND WP4 T1_V3_enviada_CSV.csv"), sep = ";", na.strings = c("NA", ""), key = "Castor Record ID")
T2 <- fread(paste0(data_path, "../target/BcnMadCE/BcnData/RESPOND WP4 T2_V3_enviada_CSV.csv"), sep = ";", na.strings = c("NA", ""), key = "Castor Record ID")
T3 <- fread(paste0(data_path, "../target/BcnMadCE/BcnData/RESPOND WP4 T3_V3_enviada_CSV.csv"), sep = ";", na.strings = c("NA", ""), key = "Castor Record ID")
T4 <- fread(paste0(data_path, "../target/BcnMadCE/BcnData/RESPOND WP4 T4_V3_enviada_CSV.csv"), sep = ";", na.strings = c("NA", ""), key = "Castor Record ID")


Mscreening <- fread(paste0(data_path, "BcnMadCE/MadData/RESPOND_-_Spain_export_20220919.csv"), sep = ";", na.strings = c("NA", ""), key = "Participant Id")
MT1 <- fread(paste0(data_path, "BcnMadCE/MadData/RESPOND_-_Spain_RESPOND_WP4_T1_export_20220818(1).csv"), sep = ";", na.strings = c("NA", ""), key = "Castor Record ID")
MT2 <- fread(paste0(data_path, "BcnMadCE/MadData/RESPOND_-_Spain_RESPOND_WP4_T2_export_20220818(1).csv"), sep = ";", na.strings = c("NA", ""), key = "Castor Record ID")
MT3 <- fread(paste0(data_path, "BcnMadCE/MadData/RESPOND_-_Spain_RESPOND_WP4_T3_export_20220818(1).csv"), sep = ";", na.strings = c("NA", ""), key = "Castor Record ID")
MT4 <- fread(paste0(data_path, "BcnMadCE/MadData/RESPOND_-_Spain_RESPOND_WP4_T4_export_20220818(1).csv"), sep = ";", na.strings = c("NA", ""), key = "Castor Record ID")


items <- fread(paste0(data_path, "BcnMadCE/survey_variablelist.csv"), encoding = "UTF-8", na.strings = c("NA", ""))

MO <- c(0, 0.084, 0.099, 0.249, 0.337)
SC <- c(0, 0.050, 0.053, 0.164, 0.196)
UA <- c(0, 0.044, 0.049, 0.135, 0.153)
PD <- c(0, 0.078, 0.101, 0.245, 0.382)
AD <- c(0, 0.081, 0.128, 0.270, 0.348)

# Pre-operations --------------------------------------------------------------

setnames(screening, \(.x) gsub("\\s", "_", .x))
setnames(T1, \(.x) gsub("\\s", "_", .x))
setnames(T1, "Survey_Completed_On", "t1_Survey_Completed_On")
setnames(T1, \(.x) sub("covid19_baseline_0(\\d)$", "covid19_\\1", .x))
setnames(T1, \(.x) sub("covid19_baseline", "covid19", .x))
setnames(T2, \(.x) gsub("\\s", "_", .x))
setnames(T2, "Survey_Completed_On", "t2_Survey_Completed_On")
setnames(T2, \(.x) sub("covid19_baseline", "covid19", .x))
setnames(T3, \(.x) gsub("\\s", "_", .x))
setnames(T3, "Survey_Completed_On", "t3_Survey_Completed_On")
setnames(T3, \(.x) sub("covid19_baseline", "covid19", .x))
setnames(T4, \(.x) gsub("\\s", "_", .x))
setnames(T4, "Survey_Completed_On", "t4_Survey_Completed_On")
setnames(T4, \(.x) sub("covid19_baseline", "covid19", .x))


setnames(Mscreening, \(.x) gsub("V", "X", .x))
setnames(Mscreening, \(.x) gsub("Site", "Institute", .x))
setnames(Mscreening, \(.x) gsub("Participant", "Record", .x))
setnames(Mscreening, \(.x) gsub("\\s", "_", .x))
setnames(MT1, \(.x) gsub("\\s", "_", .x))
setnames(MT1, "Survey_Completed_On", "t1_Survey_Completed_On")
setnames(MT1, \(.x) sub("covid19_baseline_0(\\d)$", "covid19_\\1", .x))
setnames(MT1, \(.x) sub("covid19_baseline", "covid19", .x))
setnames(MT2, \(.x) gsub("\\s", "_", .x))
setnames(MT2, "Survey_Completed_On", "t2_Survey_Completed_On")
setnames(MT2, \(.x) sub("covid19_baseline", "covid19", .x))
setnames(MT3, \(.x) gsub("\\s", "_", .x))
setnames(MT3, "Survey_Completed_On", "t3_Survey_Completed_On")
setnames(MT4, \(.x) gsub("\\s", "_", .x))
setnames(MT3, \(.x) sub("covid19_baseline", "covid19", .x))
setnames(MT4, "Survey_Completed_On", "t4_Survey_Completed_On")
setnames(MT4, \(.x) sub("covid19_baseline", "covid19", .x))


MT1 <- MT1[, .SD, .SDcols = c(grep("^t1_", names(MT1), value = TRUE), "Castor_Record_ID")]
MT2 <- MT2[, .SD, .SDcols = c(grep("^t2_", names(MT2), value = TRUE), "Castor_Record_ID")]
MT3 <- MT3[, .SD, .SDcols = c(grep("^t3_", names(MT3), value = TRUE), "Castor_Record_ID")]
MT4 <- MT4[, .SD, .SDcols = c(grep("^t4_", names(MT4), value = TRUE), "Castor_Record_ID")]



items <- items[, .(`Survey name`, `Step name`, `Variable name`, `Optiongroup name`, `Field label`)]
items[, `:=` (var = fifelse(`Survey name` != "MASTER", sub("^t\\d_", "", `Variable name`), `Variable name`),
	      `Step name` = fcase(`Step name` == "RESPOND-adapted BTQ", "BTQ",
				  `Step name` == "COVID-19-adapted CSRI (WP4)", "CSRI",
				  grepl("^COVID-19-related", `Step name`), "COVID-19",
				  rep(TRUE, .N), sub("(^[^ ]*)\\s.*$", "\\1", `Step name`)
				  ))
      ][, `:=` (var = fcase(`Step name` == "COVID-19" & var == "covid19_baseline_01_1", "covid19_01_1",
		            `Step name` == "COVID-19", sub("baseline_0", "", var),
			    rep(TRUE, .N), var),
	        `Step name` = sub(" \\(baseline\\)", "", `Step name`))]
items <- na.omit(items, cols = "var")
setcolorder(items, "var", before = "Variable name")
# uniqueN(items, by = "var") == uniqueN(items[`Survey name` == "MASTER"], by = "var")
# uniqueN(items, by = "var") == uniqueN(items[`Survey name` != "MASTER"], by = "var") + 1
# setdiff(unique(items$var), unique(items[`Survey name` != "MASTER"]$var))
# uniqueN(items, by = c("var", "Step name")) == uniqueN(items, by = c("var"))
# unique(items[, .(var, `Step name`)], by = c("var", "Step name"))[order(var)] |> utils::View()
items <- items[`Survey name` != "MASTER"]
items <- unique(items[, .(var, `Step name`, `Optiongroup name`)])

# previous arrangements to melting data
cols <- c("Randomized_On", "Record_Creation_Date")
screening[, (cols) := lapply(.SD, as.POSIXct, format = "%d/%m/%Y %k:%M"), .SDcols = cols]
T1[, `:=` (t1_csri_sp_off_heath_t = as.numeric(t1_csri_sp_off_heath_t))]
T4[, `:=` (t4_m_T1_CSRI_SP_nurse_g_t = as.integer(t4_m_T1_CSRI_SP_nurse_g_t), t4_csri_sp_off_heath_t = as.numeric(t4_csri_sp_off_heath_t))]
Mscreening[, `:=` (Record_Creation_Date = as.POSIXct(Record_Creation_Date, format = "%d-%m-%Y %H:%M:%S"))]
MT3[, `:=` (t3_m_T1_CSRI_SP_mental_g_t = as.integer(t3_m_T1_CSRI_SP_mental_g_t))]

T12 <- merge(T1, T2, all = TRUE)
T34 <- merge(T3, T4, all = TRUE)
Tdata <- merge(T12, T34, all = TRUE)


MT12 <- merge(MT1, MT2, all = TRUE)
MT34 <- merge(MT3, MT4, all = TRUE)
MTdata <- merge(MT12, MT34, all = TRUE)


Tlong <- melt(Tdata, 
	      measure.vars = measure(wave = as.integer, value.name, pattern = "^t([1234])_(?!t[01]_)(.*)"))
Tlong <- na.omit(Tlong, cols = "Survey_Completed_On")
Tlong[, `:=` (Survey_Completed_On = as.POSIXct(Survey_Completed_On, format = "%d/%m/%Y %k:%M"))]

MTlong <- melt(MTdata, 
	       measure.vars = measure(wave = as.integer, value.name, pattern = "^t([1234])_(?!t[01]_)(.*)"))
MTlong <- na.omit(MTlong, cols = "Survey_Completed_On")
MTlong[, `:=` (Survey_Completed_On = as.POSIXct(Survey_Completed_On, format = "%d-%m-%Y %H:%M:%S"))]

Tlong <- rbind(Tlong, MTlong)
screening <- rbind(screening, Mscreening)
setkey(Tlong, Castor_Record_ID, wave)
setnames(Tlong, \(.x) sub("^t1_(t[01])", "\\1", .x))


cols <- c("Randomization_Group", "Institute_Abbreviation")
screening[, (cols) := lapply(.SD, factor), .SDcols = cols]










# names(Tlong)[names(Tlong) %in% items$var] |> identical(items[var != "eq5d5l_remark"]$var)
# names(Tlong)[names(Tlong) %in% items$var] <- paste0(names(Tlong)[names(Tlong) %in% items$var], " [", items[var != "eq5d5l_remark"]$`Step name`,"]")
enTlong <- paste0(names(Tlong)[names(Tlong) %in% items$var], " [", items[var != "eq5d5l_remark"]$`Step name`,"]")

# Data outcomes -----------------------------------------------------------


Tlong[, phq9 := rowSums2(as.matrix(.SD)), .SDcols = patterns("^phq9_0\\d")
      ][, gad7 := rowSums2(as.matrix(.SD)), .SDcols = patterns("^gad7_\\d")
      ][, ptsd := rowSums2(as.matrix(.SD)), .SDcols = patterns("^pcl5_\\d")
      ][, phq_ads := phq9 + gad7
      ][, passc := rowSums2(as.matrix(.SD)), .SDcols = patterns("passc_\\d+")
      ][, ]





eTlong <- merge(screening, Tlong, all = TRUE, by.x = "Record_Id", by.y = "Castor_Record_ID")

save(Tdata, MTdata, Tlong, enTlong, screening, eTlong, file = paste0(data_path, "../target/BcnMadCE/CEdata.rdata"))
