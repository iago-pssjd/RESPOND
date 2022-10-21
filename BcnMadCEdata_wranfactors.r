# BcnMadCEdata_wranfactors.r
# from BcnMadCEdata_SR.r
# to BcnMadCE.r

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
library(openxlsx)

# Data loading --------------------------------------------------------------


load(paste0(data_path, "../target/BcnMadCE/CEdataSR.rdata"))
items <- fread(paste0(data_path, "BcnMadCE/survey_variablelist.csv"), encoding = "UTF-8", na.strings = c("NA", ""))
fields <- fread(paste0(data_path, "BcnMadCE/field_options.csv"), encoding = "UTF-8", na.strings = c("NA", ""))
pmMad <- fread(paste0(data_path, "BcnMadCE/MadData/pm_attendance.csv"), encoding = "UTF-8", na.strings = c("NA", ""))
pmBcn <- read.xlsx(paste0(data_path, "BcnMadCE/BcnData/Num sesiones PM.xlsx"))
dwmMad <- fread(paste0(data_path, "BcnMadCE/MadData/Respond Aid Workers_user_activity_06-09-2022 12 14 46.csv"), encoding = "UTF-8", na.strings = c("NA", ""))
dwmBcn <- fread(paste0(data_path, "BcnMadCE/BcnData/Meta-data app_v2_BCN.csv"), encoding = "UTF-8", na.strings = c("NA", ""))

# Operations --------------------------------------------------------------


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
items <- items[`Survey name` != "MASTER"]
items <- unique(items[, .(var, `Step name`, `Optiongroup name`)])

metadata <- merge(na.omit(items, cols = c("var", "Optiongroup name")), fields, all = FALSE, by.x = "Optiongroup name", by.y = "Option group name", allow.cartesian = TRUE)
metadata <- metadata[, .(var, `Option name`, `Option value`)]
setnames(metadata, \(.x) sub("Option ", "", .x))




val2name <- split(metadata, by = "var", keep.by = FALSE)
invisible(lapply(val2name, \(.x) .x[, name := factor(name, levels = name)]))

for(var in names(val2name)){
	Tlong[val2name[[var]], on = paste0(var, "==value"), name := i.name]
	Tlong[, (var) := name][, name := NULL]
}



setnames(pmBcn, old = names(pmBcn), new = c("Record_Id", "pmN"))
setnames(pmMad, old = names(pmMad), new = c("Record_Id", "pmN"))

pmp <- rbind(pmMad, pmBcn)
pmp[, Record_Id := gsub(" ", "", Record_Id)]

dwmMad[, researchID := gsub("^UAM", "ES-UAM", researchID)][, dwmN := rowSums2(as.matrix(.SD)), .SDcols = `finished: Grounding`:`finished: Making room`] # "ES-UAM-0001" is a control
dwmMad <- dwmMad[grepl("^ES-UAM-\\d{4}$", researchID), .(researchID, dwmN)]
dwmBcn[grepl("ES-SJD-\\d{4}", researchID), researchID := gsub(" ", "", researchID)][, dwmN := rowSums2(as.matrix(.SD)), .SDcols = `finished: Grounding`:`finished: Making room`]
dwmBcn <- unique(dwmBcn[grepl("^ES-SJD-\\d{4}$", researchID), .(researchID, dwmN)])
dwm <- rbind(dwmBcn, dwmMad)

screening <- merge(screening, pmp, all = TRUE)
screening <- merge(screening, dwm, all = TRUE, by.x = "Record_Id", by.y = "researchID")

eTlong <- merge(screening, Tlong, all = TRUE, by.x = "Record_Id", by.y = "Castor_Record_ID")


save(Tdata, MTdata, Tlong, enTlong, screening, eTlong, file = paste0(data_path, "../target/BcnMadCE/CEdata2.rdata"))
