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


items <- fread(paste0(data_add, "../../survey_variablelist.csv"), encoding = "UTF-8", na.strings = c("NA", ""))
items <- items[, .(`Survey name`, `Step name`, `Variable name`, `Optiongroup name`, `Field label`)]



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
        ][, ptsd := rowSums2(as.matrix(.SD)), .SDcols = patterns("^pcl5_\\d")
          ][, phq_ads := phq9 + gad7]

outcomes <- c("phq_ads" = "phq9_0|gad7_", "phq9" = "phq9_0", "gad7" = "gad7_", "ptsd" = "pcl5_")
rbindlist(lapply(seq_along(outcomes), \(.x){
  out <- names(outcomes)[.x]
  Tlong[, .(measName = out,
            avg = mean(.SD[[out]], na.rm = TRUE), 
            lower = t.test(.SD[[out]])$conf.int[1], 
            upper = t.test(.SD[[out]])$conf.int[2],
            pval = t.test(.SD[[out]])$p.value,
            alpha = cronbachs_alpha(.SD[, -out, with = FALSE])), 
        by = .(wave), 
        .SDcols = patterns(paste0("^((",outcomes[.x],")\\d|",names(outcomes)[.x],"$)"))]
}))





eTlong <- merge(screening, Tlong, all = TRUE, by.x = "Record_Id", by.y = "Castor_Record_ID")

eTlong[order(Randomization_Group, wave), .(avg_phq_ads = mean(phq_ads, na.rm = TRUE)), by = .(wave, Randomization_Group)]
eS1 <- eTlong[order(Institute_Abbreviation, Randomization_Group, wave), 
       unlist(lapply(.SD, \(.x) list(mean = mean(.x, na.rm = TRUE),
                                     sd = sd(.x, na.rm = TRUE),
                                     median = median(.x, na.rm = TRUE),
                                     Q1 = quantile(.x, probs = 0.25, na.rm = TRUE),
                                     Q3 = quantile(.x, probs = 0.75, na.rm = TRUE),
                                     max = max(.x, na.rm = TRUE),
                                     min = min(.x, na.rm = TRUE))), 
              rec = FALSE), 
       by = .(Institute_Abbreviation), .SDcols = c("phq_ads", "phq9", "gad7")]

melt(eS1, measure.vars = measure(outcome, value.name, pattern = "(^.*)\\.([a-zA-Z0-9]*)"))

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
