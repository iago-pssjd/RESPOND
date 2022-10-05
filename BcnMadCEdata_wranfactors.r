# BcnMadCEdata_wranfactors.r
# from BcnMadCEdata_SR.r
# to BcnMadCE.r

# Data directory ----------------------------------------------------------


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



# Libraries ---------------------------------------------------------------



library(matrixStats)
library(data.table)
library(openxlsx)

# Data loading --------------------------------------------------------------


load(paste0(data_add, "../target/BcnMadCE/CEdataSR.rdata"))
items <- fread(paste0(data_add, "BcnMadCE/survey_variablelist.csv"), encoding = "UTF-8", na.strings = c("NA", ""))
fields <- fread(paste0(data_add, "BcnMadCE/field_options.csv"), encoding = "UTF-8", na.strings = c("NA", ""))
pmMad <- fread(paste0(data_add, "BcnMadCE/MadData/pm_attendance.csv"), encoding = "UTF-8", na.strings = c("NA", ""))
pmBcn <- read.xlsx(paste0(data_add, "BcnMadCE/BcnData/Num sesiones PM.xlsx"))
dwm <- fread(paste0(data_add, "BcnMadCE/Respond Aid Workers_user_activity_06-09-2022 12 14 46.csv"), encoding = "UTF-8", na.strings = c("NA", ""))

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

dwm[, researchID := gsub("^UAM", "ES-UAM", gsub("UAM(\\d{4})","UAM-\\1", researchID))][, dwmN := rowSums2(as.matrix(.SD)), .SDcols = `finished: Grounding`:`finished: Making room`]
dwm <- unique(dwm[grepl("^ES-(UAM|SJD)-\\d{4}$", researchID), .(researchID, dwmN)], by = "researchID") # second row of ES-SJD-0049 is all 0




screening <- merge(screening, pmp, all = TRUE)
screening <- merge(screening, dwm, all = TRUE, by.x = "Record_Id", by.y = "researchID")

eTlong <- merge(screening, Tlong, all = TRUE, by.x = "Record_Id", by.y = "Castor_Record_ID")


save(Tdata, MTdata, Tlong, enTlong, screening, eTlong, file = paste0(data_add, "../target/BcnMadCE/CEdata2.rdata"))
