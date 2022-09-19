# BcnMadCE.r

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



library(ggplot2)
library(performance)
library(car)
library(systemfit)
library(matrixStats)
library(data.table)





# Metadata ----------------------------------------------------------------

fields <- fread(paste0(data_add, "../../field_options.csv"), encoding = "UTF-8", na.strings = c("NA", ""))
scitems <- fread(paste0(data_add, "../../study_variablelist.csv"), encoding = "UTF-8", na.strings = c("NA", ""))
scitems <- na.omit(scitems[, .(`Variable name`, `Optiongroup name`)])
items <- fread(paste0(data_add, "../../survey_variablelist.csv"), encoding = "UTF-8", na.strings = c("NA", ""))
items <- na.omit(items[, .(`Variable name`, `Optiongroup name`)])
scmetadata <- merge(scitems, fields, all = FALSE, by.x = "Optiongroup name", by.y = "Option group name", allow.cartesian = TRUE)
metadata <- merge(items, fields, all = FALSE, by.x = "Optiongroup name", by.y = "Option group name", allow.cartesian = TRUE)


# Bcn data recoding ------------------------------------------------------------


## screening ---------------------------------------------------------------



screening <- readLines(paste0(data_add, "BcnMadCE/BcnData/BCN_Spain_export_20220906_v2.csv"))
# table(sapply(gregexpr(";", screening), length))
# table(sapply(gregexpr(",", screening), length))
# gregexpr(",", screening) |> unlist() |> table()
# screening <- gsub(",", ";", screening)
screening <- fread(text = screening, sep = ";", na.strings = c("NA", ""), key = "Record Id")


### checks ------------------------------------------------------------------


# scmetadata[`Variable name` %in% names(screening)] |> utils::View()
# scmetadata[`Variable name` %in% names(screening), .(`Variable name`)] |> unique()
scfields <- names(screening)[names(screening) %in% scmetadata$`Variable name`]
# Checks for non scfields
# screening[, !scfields, with = FALSE]
# table(screening$`Record Status`)
# table(screening$`Institute Abbreviation`)
# screening[, !scfields, with = FALSE][, lapply(.SD, \(.x) sum(is.na(.x)))]
# Tests for scfields
## lapply(screening[, scfields, with = FALSE], table, useNA = "always")
## screening[, lapply(.SD, table, useNA = "always"), .SDcols = scfields]
# Main check
# table(sapply(scfields, \(.x) length(setdiff(names(table(screening[[.x]])), scmetadata[`Variable name` == .x]$`Option value`)) == 0))


## T1 ---------------------------------------------------------------



T1 <- readLines(paste0(data_add, "BcnMadCE/BcnData/RESPOND WP4 T1_V2_enviada_CSV.csv"))
# table(sapply(gregexpr(";", T1), length))
# table(sapply(gregexpr(",", T1), length))
# gregexpr(",", T1) |> unlist() |> table()
# T1 <- gsub(",", ";", T1)
T1 <- fread(text = T1, sep = ";", na.strings = c("NA", ""))

### checks ------------------------------------------------------------------
# metadata[`Variable name` %in% names(T1)][order(`Optiongroup name`, `Variable name`, `Option value`)] |> utils::View()

T1fields <- names(T1)[names(T1) %in% metadata$`Variable name`]
# Checks for non T1fields
# T1[, !T1fields, with = FALSE]
# Main check
# sapply(T1fields, \(.x) length(setdiff(names(table(T1[[.x]])), metadata[`Variable name` == .x]$`Option value`)) == 0)

sinocols <- grep("^t1_", unique(metadata[grep("(?i)si(\\s|/)no", unique(metadata$`Optiongroup name`), value = TRUE)]$`Variable name`), value = TRUE, perl = TRUE)
m1cols <- c(paste0("t1_", c("t1_soc_2", "t0_soc_12")),
            grep("^t1_", unique(metadata[c(grep("(?i)covid|ph|gad|pcl-5|csri", unique(metadata$`Optiongroup name`), value = TRUE),
                                           "MIMIS core and WP specific stressor list spain", "Major stressors SP")]$`Variable name`), value = TRUE),
            NULL)

T1[, c(sinocols) := lapply(.SD, \(.x) 2L - .x), .SDcols = sinocols
   ][, c(m1cols) := lapply(.SD, \(.x) .x - 1L), .SDcols = m1cols]

qualtrics2castor <- list(t1_t0_soc_01 = data.table(from = 1:6, to = c(1L,2L,5L,6L,3L,4L)),
                         t1_t1_soc_1 = data.table(from = 1:5, to = c(0L,1L,4L,5L,6L)))

for(var in names(qualtrics2castor)){
  T1[qualtrics2castor[[var]], on = paste0(var, "==from"), (var) := i.to]
}

fwrite(T1, file = paste0(data_add, "../target/BcnMadCE/BcnData/RESPOND WP4 T1_V3_enviada_CSV.csv"), sep = ";")



## T2 ---------------------------------------------------------------

T2 <- readLines(paste0(data_add, "BcnMadCE/BcnData/RESPOND WP4 T2_V2_enviada_CSV.csv"))
# table(sapply(gregexpr(";", T2), length))
# table(sapply(gregexpr(",", T2), length))
# gregexpr(",", T2) |> unlist() |> table()
T2 <- gsub(",", ".", T2)
T2 <- fread(text = T2, sep = ";", na.strings = c("NA", ""))


### checks ------------------------------------------------------------------
# metadata[`Variable name` %in% names(T2)][order(`Optiongroup name`, `Variable name`, `Option value`)] |> utils::View()

T2fields <- names(T2)[names(T2) %in% metadata$`Variable name`]
# Checks for non T2fields
# T2[, !T2fields, with = FALSE]
# Main check
# sapply(T2fields, \(.x) length(setdiff(names(table(T2[[.x]])), metadata[`Variable name` == .x]$`Option value`)) == 0)

m1cols <- grep("^t2_", unique(metadata[c(grep("(?i)ph|gad|covid|pcl-5|csri", unique(metadata$`Optiongroup name`), value = TRUE),
                                         "MIMIS core and WP specific stressor list spain", "Major stressors SP")]$`Variable name`), value = TRUE)
sinocols <- grep("^t2_", unique(metadata[grep("(?i)si(\\s|/)no", unique(metadata$`Optiongroup name`), value = TRUE)]$`Variable name`), value = TRUE, perl = TRUE)

T2[, c(sinocols) := lapply(.SD, \(.x) 2L - .x), .SDcols = sinocols
   ][, c(m1cols) := lapply(.SD, \(.x) .x - 1L), .SDcols = m1cols]


fwrite(T2, file = paste0(data_add, "../target/BcnMadCE/BcnData/RESPOND WP4 T2_V3_enviada_CSV.csv"), sep = ";")


## T3 ---------------------------------------------------------------

T3 <- readLines(paste0(data_add, "BcnMadCE/BcnData/RESPOND WP4 T3_V2_enviada_CSV.csv"))
# table(sapply(gregexpr(";", T3), length))
# table(sapply(gregexpr(",", T3), length))
# gregexpr(",", T3) |> unlist() |> table()
T3 <- gsub(",", ".", T3)
T3 <- fread(text = T3, sep = ";", na.strings = c("NA", ""))



### checks ------------------------------------------------------------------
# metadata[`Variable name` %in% names(T3)][order(`Optiongroup name`, `Variable name`, `Option value`)] |> utils::View()

T3fields <- names(T3)[names(T3) %in% metadata$`Variable name`]
# Checks for non T3fields
# T3[, !T3fields, with = FALSE]
# Main check
# sapply(T3fields, \(.x) length(setdiff(names(table(T3[[.x]])), metadata[`Variable name` == .x]$`Option value`)) == 0)

m1cols <- grep("^t3_", unique(metadata[c(grep("(?i)ph|gad|covid|pcl-5|csri", unique(metadata$`Optiongroup name`), value = TRUE),
                                         "MIMIS core and WP specific stressor list spain", "Major stressors SP")]$`Variable name`), value = TRUE)
sinocols <- grep("^t3_", unique(metadata[grep("(?i)si(\\s|/)no", unique(metadata$`Optiongroup name`), value = TRUE)]$`Variable name`), value = TRUE, perl = TRUE)

T3[, c(sinocols) := lapply(.SD, \(.x) 2L - .x), .SDcols = sinocols
   ][, c(m1cols) := lapply(.SD, \(.x) .x - 1L), .SDcols = m1cols]


fwrite(T3, file = paste0(data_add, "../target/BcnMadCE/BcnData/RESPOND WP4 T3_V3_enviada_CSV.csv"), sep = ";")






## T4 ---------------------------------------------------------------

T4 <- readLines(paste0(data_add, "BcnMadCE/BcnData/RESPOND WP4 T4_V2_enviada_CSV.csv"))
# table(sapply(gregexpr(";", T4), length))
# table(sapply(gregexpr(",", T4), length))
# gregexpr(",", T4) |> unlist() |> table()
# T4 <- gsub(",", ";", T4)
T4 <- fread(text = T4, sep = ";", na.strings = c("NA", ""))



### checks ------------------------------------------------------------------
# metadata[`Variable name` %in% names(T4)][order(`Optiongroup name`, `Variable name`, `Option value`)] |> utils::View()

T4fields <- names(T4)[names(T4) %in% metadata$`Variable name`]
# Checks for non T4fields
# T4[, !T4fields, with = FALSE]
# Main check
# sapply(T4fields, \(.x) length(setdiff(names(table(T4[[.x]])), metadata[`Variable name` == .x]$`Option value`)) == 0)

m1cols <- grep("^t4_", unique(metadata[c(grep("(?i)ph|gad|covid|pcl-5|csri", unique(metadata$`Optiongroup name`), value = TRUE),
                                         "MIMIS core and WP specific stressor list spain", "Major stressors SP")]$`Variable name`), value = TRUE)
sinocols <- grep("^t4_", unique(metadata[grep("(?i)si(\\s|/)no", unique(metadata$`Optiongroup name`), value = TRUE)]$`Variable name`), value = TRUE, perl = TRUE)

T4[, c(sinocols) := lapply(.SD, \(.x) 2L - .x), .SDcols = sinocols
   ][, c(m1cols) := lapply(.SD, \(.x) .x - 1L), .SDcols = m1cols]


fwrite(T4, file = paste0(data_add, "../target/BcnMadCE/BcnData/RESPOND WP4 T4_V3_enviada_CSV.csv"), sep = ";")






# Data loading --------------------------------------------------------------

screening <- fread(paste0(data_add, "BcnMadCE/BcnData/BCN_Spain_export_20220906_v2.csv"), sep = ";", na.strings = c("NA", ""), key = "Record Id")
T1 <- fread(paste0(data_add, "../target/BcnMadCE/BcnData/RESPOND WP4 T1_V3_enviada_CSV.csv"), sep = ";", na.strings = c("NA", ""), key = "Castor Record ID")
T2 <- fread(paste0(data_add, "../target/BcnMadCE/BcnData/RESPOND WP4 T2_V3_enviada_CSV.csv"), sep = ";", na.strings = c("NA", ""), key = "Castor Record ID")
T3 <- fread(paste0(data_add, "../target/BcnMadCE/BcnData/RESPOND WP4 T3_V3_enviada_CSV.csv"), sep = ";", na.strings = c("NA", ""), key = "Castor Record ID")
T4 <- fread(paste0(data_add, "../target/BcnMadCE/BcnData/RESPOND WP4 T4_V3_enviada_CSV.csv"), sep = ";", na.strings = c("NA", ""), key = "Castor Record ID")


Mscreening <- fread(paste0(data_add, "BcnMadCE/MadData/RESPOND_-_Spain_export_20220919.csv"), sep = ";", na.strings = c("NA", ""), key = "Participant Id")
MT1 <- fread(paste0(data_add, "BcnMadCE/MadData/RESPOND_-_Spain_RESPOND_WP4_T1_export_20220818(1).csv"), sep = ";", na.strings = c("NA", ""), key = "Castor Record ID")
MT2 <- fread(paste0(data_add, "BcnMadCE/MadData/RESPOND_-_Spain_RESPOND_WP4_T2_export_20220818(1).csv"), sep = ";", na.strings = c("NA", ""), key = "Castor Record ID")
MT3 <- fread(paste0(data_add, "BcnMadCE/MadData/RESPOND_-_Spain_RESPOND_WP4_T3_export_20220818(1).csv"), sep = ";", na.strings = c("NA", ""), key = "Castor Record ID")
MT4 <- fread(paste0(data_add, "BcnMadCE/MadData/RESPOND_-_Spain_RESPOND_WP4_T4_export_20220818(1).csv"), sep = ";", na.strings = c("NA", ""), key = "Castor Record ID")






# Pre-operations --------------------------------------------------------------

setnames(screening, \(.x) gsub("\\s", "_", .x))
setnames(T1, \(.x) gsub("\\s", "_", .x))
setnames(T1, "Survey_Completed_On", "t1_Survey_Completed_On")
setnames(T2, \(.x) gsub("\\s", "_", .x))
setnames(T2, "Survey_Completed_On", "t2_Survey_Completed_On")
setnames(T3, \(.x) gsub("\\s", "_", .x))
setnames(T3, "Survey_Completed_On", "t3_Survey_Completed_On")
setnames(T4, \(.x) gsub("\\s", "_", .x))
setnames(T4, "Survey_Completed_On", "t4_Survey_Completed_On")


setnames(Mscreening, \(.x) gsub("V", "X", .x))
setnames(Mscreening, \(.x) gsub("Site", "Institute", .x))
setnames(Mscreening, \(.x) gsub("Participant", "Record", .x))
setnames(Mscreening, \(.x) gsub("\\s", "_", .x))
setnames(MT1, \(.x) gsub("\\s", "_", .x))
setnames(MT1, "Survey_Completed_On", "t1_Survey_Completed_On")
setnames(MT2, \(.x) gsub("\\s", "_", .x))
setnames(MT2, "Survey_Completed_On", "t2_Survey_Completed_On")
setnames(MT3, \(.x) gsub("\\s", "_", .x))
setnames(MT3, "Survey_Completed_On", "t3_Survey_Completed_On")
setnames(MT4, \(.x) gsub("\\s", "_", .x))
setnames(MT4, "Survey_Completed_On", "t4_Survey_Completed_On")


MT1 <- MT1[, .SD, .SDcols = c(grep("^t1_", names(MT1), value = TRUE), "Castor_Record_ID")]
MT2 <- MT2[, .SD, .SDcols = c(grep("^t2_", names(MT2), value = TRUE), "Castor_Record_ID")]
MT3 <- MT3[, .SD, .SDcols = c(grep("^t3_", names(MT3), value = TRUE), "Castor_Record_ID")]
MT4 <- MT4[, .SD, .SDcols = c(grep("^t4_", names(MT4), value = TRUE), "Castor_Record_ID")]


# previous arrangements to melting data
screening[, `:=` (Randomized_On = as.POSIXct(Randomized_On))]
T1[, `:=` (t1_csri_sp_off_heath_t = as.numeric(t1_csri_sp_off_heath_t))]
T4[, `:=` (t4_m_T1_CSRI_SP_nurse_g_t = as.integer(t4_m_T1_CSRI_SP_nurse_g_t), t4_csri_sp_off_heath_t = as.numeric(t4_csri_sp_off_heath_t))]
MT3[, `:=` (t3_m_T1_CSRI_SP_mental_g_t = as.integer(t3_m_T1_CSRI_SP_mental_g_t))]

T12 <- merge(T1, T2, all = TRUE)
T34 <- merge(T3, T4, all = TRUE)
Tdata <- merge(T12, T34, all = TRUE)


MT12 <- merge(MT1, MT2, all = TRUE)
MT34 <- merge(MT3, MT4, all = TRUE)
MTdata <- merge(MT12, MT34, all = TRUE)


Tlong <- melt(Tdata, 
            measure.vars = measure(wave = as.integer, value.name, pattern = "^t([1234])_(?<!t1_t[01]_)(.*)"))
Tlong <- na.omit(Tlong, cols = "Survey_Completed_On")


MTlong <- melt(MTdata, 
              measure.vars = measure(wave = as.integer, value.name, pattern = "^t([1234])_(?<!t1_t[01]_)(.*)"))
MTlong <- na.omit(MTlong, cols = "Survey_Completed_On")

Tlong <- rbind(Tlong, MTlong)
screening <- rbind(screening, Mscreening)

# Data outcomes -----------------------------------------------------------


Tlong[, phq9 := rowSums2(as.matrix(.SD)), .SDcols = patterns("^phq9_0\\d")
      ][, gad7 := rowSums2(as.matrix(.SD)), .SDcols = patterns("^gad7_\\d")
        ][, phq_ads := phq9 + gad7]


Tlong[, .(avg_phq_ads = mean(phq_ads, na.rm = TRUE), 
          lower = t.test(phq_ads)$conf.int[1], 
          upper = t.test(phq_ads)$conf.int[2],
          pval = t.test(phq_ads)$p.value,
          alpha = cronbachs_alpha(.SD)), by = .(wave), .SDcols = patterns("^(phq9_0|gad7_)\\d")]




eTlong <- merge(screening, Tlong, all = TRUE, by.x = "Record_Id", by.y = "Castor_Record_ID")

eTlong[order(Randomization_Group, wave), .(avg_phq_ads = mean(phq_ads, na.rm = TRUE)), by = .(wave, Randomization_Group)]



eTlong[, lapply(.SD, mean, na.rm = TRUE), .SDcols = c("phq_ads"), by = .(wave, Randomization_Group)] |> 
  ggplot(aes(x = wave, y = phq_ads, colour = Randomization_Group)) +
  geom_line()

na.omit(eTlong, cols = "phq_ads") |> 
  ggplot(aes(x = factor(wave), y = phq_ads, colour = Randomization_Group)) +
  stat_summary(aes(group = Randomization_Group), geom = "line", fun = mean) +
  stat_summary(aes(group = Randomization_Group), geom = "point", fun = mean) +
  stat_boxplot(geom = "errorbar", position = "identity", width = 0.25) +
  labs(x = "time")


# table(screening$Randomization_Group)
# uniqueN(eTlong, by = "Record_Id")
# Tlong[order(Castor_Record_ID, wave)] |> str()
