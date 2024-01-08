# BcnMadCEdata_arr.r
# to BcnMadCEdata_wran.r

#R! Data directory


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



#R! Libraries



library(matrixStats)
library(data.table)
library(openxlsx2)




#R! Metadata

fields <- fread(paste0(data_add, "BcnMadCE/field_options.csv"), encoding = "UTF-8", na.strings = c("NA", ""))
#scitems <- fread(paste0(data_add, "BcnMadCE/study_variablelist.csv"), encoding = "UTF-8", na.strings = c("NA", ""))
items <- fread(paste0(data_add, "BcnMadCE/survey_variablelist.csv"), encoding = "UTF-8", na.strings = c("NA", ""))
#scmetadata <- merge(scitems, fields, all = FALSE, by.x = "Optiongroup name", by.y = "Option group name", allow.cartesian = TRUE)




#R!! Operations

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

#R! REPICAL data loading


dataW <- read_xlsx(paste0(data_add, 'REPICAL/BBDDRepical_pre.xlsx'))
setDT(dataW)

#R! Pre-processing


#R!! Fix dataset

setnames(dataW, old = c("t2_phq9_item10", "t3_phq_item10", "t4_phq9_item10", paste0("t1_covid19_0", 1:4), "SJD_ID", "Grupo", "K10_tot2"), new = c(paste0(paste0("t", 2:4), "_phq9_10"), paste0("t1_covid19_", 1:4), "Record_ID", "Randomization_Group", "t2_k10_score"))
#1 = Intervention
#2 = Control


#R!! Reshape dataset

dataL <- melt(dataW, measure.vars = measure(wave = as.integer, value.name, pattern = "^t([1234])_(?!t[01]_)(.*)"))
setkey(dataL, Record_ID, wave)
setnames(dataL, \(.x) sub("^t1_(t[01])", "\\1", .x))



write_xlsx(dataL[order(wave, Record_ID)], paste0(data_add, "../target/REPICAL/BBDDRepical_long.xlsx"), na.strings = "")


setnames(dataL, \(.x) gsub("^(btq|phq9|pass|k10)_([123456789])$", "\\1_0\\2", .x))
setnames(dataL, \(.x) gsub("^csri_(.*)$", "csri_sp_\\1", .x))
setnames(dataL, \(.x) gsub("^m_CSRI_(.*)$", "m_T1_CSRI_SP_\\1", .x))



cols <- c("Randomization_Group")
dataL[, (cols) := lapply(.SD, factor), .SDcols = cols]


write_xlsx(dataL[order(wave, Record_ID)], paste0(data_add, "../target/REPICAL/BBDDRepical_long_modifiedNames.xlsx"), na.strings = "")



#R!! Removing variables

## Not in Respond Clinical Essay with those names and unnecessary
#dataL <- dataL[, !c("Login_ID", "K10_tot2")]

## In Respond Clinical Essay but uncertain categories/codes and unnecessary
#dataL <- dataL[, !c("t1_soc_01_1", "t1_soc_1", "t1_soc_1_1")]

#t0_soc_02 = year of birth
#t0_soc_01 = gender


#R!! Data outcomes


dataL[, phq9 := rowSums2(as.matrix(.SD)), .SDcols = patterns("^phq9_0\\d")
      ][, gad7 := rowSums2(as.matrix(.SD)), .SDcols = patterns("^gad7_\\d")
      ][, ptsd := rowSums2(as.matrix(.SD)), .SDcols = patterns("^pcl5_\\d")
      ][, phq_ads := phq9 + gad7
      ][, passc := rowSums2(as.matrix(.SD)), .SDcols = patterns("passc_\\d+")
      ][!is.na(eq5d5l_1) & !is.na(eq5d5l_2) & !is.na(eq5d5l_3) & !is.na(eq5d5l_5) & !is.na(eq5d5l_5), `:=` (EQ5D5Lds = interaction(eq5d5l_1, eq5d5l_2, eq5d5l_3, eq5d5l_4, eq5d5l_5, sep = ""), EuroQoL_index = 1 - MO[eq5d5l_1] - SC[eq5d5l_2] - UA[eq5d5l_3] - PD[eq5d5l_4] - AD[eq5d5l_5])]



metadata <- metadata[var %in% names(dataL)]
metadata <- rbind(metadata, data.table(var = rep("Randomization_Group", 2), name = c("Intervention", "Control"), value = 1:2))


val2name <- split(metadata, by = "var", keep.by = FALSE)
invisible(lapply(val2name, \(.x) .x[, name := factor(name, levels = name)]))

for(var in names(val2name)){
	dataL[val2name[[var]], on = paste0(var, "==value"), name := i.name]
	dataL[, (var) := name][, name := NULL]
}


