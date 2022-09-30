# BcnMadCE.r
# from BcnMadCEdata_wranfactors.r

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
library(formattable)
library(compareGroups)
library(car)
library(systemfit)
library(clubSandwich)
library(nlme)
library(matrixStats)
library(data.table)






# Data loading --------------------------------------------------------------

load(paste0(data_add, "../target/BcnMadCE/CEdata2.rdata"))



# Data preparation  ----------------------------------------------------------------

Tlong[, `:=` (phq9_depression = factor(phq9 >= 10), gad7_anxiety = factor(gad7 >= 10))]
eTlong <- merge(screening, Tlong, all = TRUE, by.x = "Record_Id", by.y = "Castor_Record_ID")

# Analysis  ----------------------------------------------------------------

## Summaries ----------------------------------


### First approximation ----------------------------------
# outcomes <- c("phq_ads" = "phq9_0|gad7_", "phq9" = "phq9_0", "gad7" = "gad7_", "ptsd" = "pcl5_")
# ST1 <- rbindlist(lapply(seq_along(outcomes), \(.x){
# 				out <- names(outcomes)[.x]
# 				Tlong[, .(outcome = out,
# 					  avg = mean(.SD[[out]], na.rm = TRUE), 
# 					  sd = sd(.SD[[out]], na.rm = TRUE),
# 					  median = median(.SD[[out]], na.rm = TRUE),
# 					  Q1 = quantile(.SD[[out]], probs = 0.25, na.rm = TRUE),
# 					  Q3 = quantile(.SD[[out]], probs = 0.75, na.rm = TRUE),
# 					  max = max(.SD[[out]], na.rm = TRUE),
# 					  min = min(.SD[[out]], na.rm = TRUE), 
# 					  lower = t.test(.SD[[out]])$conf.int[1], 
# 					  upper = t.test(.SD[[out]])$conf.int[2],
# 					  pval = t.test(.SD[[out]])$p.value,
# 					  alpha = cronbachs_alpha(sapply(.SD[, -out, with = FALSE], \(.y) as.numeric(.y) - 1))), 
# 			by = .(wave), 
# 			.SDcols = patterns(paste0("^((",outcomes[.x],")\\d|", out,"$)"))]
# }))
# 
# 
# melt(dcast(ST1, outcome ~ wave, value.var = names(ST1[,-c("outcome", "wave")])),
#      measure.vars = measure(coef = \(.x) factor(.x, levels = names(ST1[,-c("outcome", "wave")])), 
# 			    value.name, pattern = "^([a-zA-Z0-9]+)_([1234])$"))[order(outcome, coef)]


### Similar per institution or group ----------------------------------------



# eTlong[order(Randomization_Group, wave), .(avg_phq_ads = mean(phq_ads, na.rm = TRUE)), by = .(wave, Randomization_Group)]
# 
# eS1 <- eTlong[order(Institute_Abbreviation, Randomization_Group, wave), 
# 	      unlist(lapply(.SD, \(.x) list(mean = mean(.x, na.rm = TRUE),
# 					    sd = sd(.x, na.rm = TRUE),
# 					    median = median(.x, na.rm = TRUE),
# 					    Q1 = quantile(.x, probs = 0.25, na.rm = TRUE),
# 					    Q3 = quantile(.x, probs = 0.75, na.rm = TRUE),
# 					    max = max(.x, na.rm = TRUE),
# 					    min = min(.x, na.rm = TRUE))), 
# 		     rec = FALSE), 
#               by = .(Institute_Abbreviation), .SDcols = c("phq_ads", "phq9", "gad7")]
# 
# melt(eS1, measure.vars = measure(outcome, value.name, pattern = "(^.*)\\.([a-zA-Z0-9]*)"))
# 
# eTlong[, lapply(.SD, mean, na.rm = TRUE), .SDcols = c("phq_ads"), by = .(wave, Randomization_Group)] |> 
# ggplot(aes(x = wave, y = phq_ads, colour = Randomization_Group)) +
# 	geom_line()
# 
# na.omit(eTlong, cols = "phq_ads") |> 
# ggplot(aes(x = factor(wave), y = phq_ads, colour = Randomization_Group)) +
# 	stat_summary(aes(group = Randomization_Group), geom = "line", fun = mean) +
# 	stat_summary(aes(group = Randomization_Group), geom = "point", fun = mean) +
# 	stat_boxplot(geom = "errorbar", position = "identity", width = 0.25) +
# 	labs(x = "time")



## Characteristics ---------------------------------------------------------

NpwF <- eTlong[wave == 1, .(Overall = .N)]
NpwF[, `:=` (outcome = "N", measure = "N")]
setcolorder(NpwF, c("outcome", "measure"))

NpwG <- dcast(eTlong[wave == 1, .N, by = .(Randomization_Group)], . ~ Randomization_Group, value.var = "N")
setnames(NpwG, old = ".", new = "outcome")
NpwG$outcome <- NpwG$measure <- "N"
setcolorder(NpwG, "measure", after = "outcome")

Npw <- NpwF[NpwG, on = .(outcome, measure)]


eTlong[, `:=` (age = 2022 - t0_soc_02, 
               to_soc_covid19 = fifelse(covid19_1 == "No", FALSE, TRUE), 
               t0_soc_site = fifelse(Institute_Abbreviation == "SJD", "Bcn", "Mad"))]

catOutcomes <- c("t0_soc_01", "t0_soc_12", "t0_soc_16", "t0_soc_17", "t0_soc_18", "to_soc_covid19", "t0_soc_site")


eT1F <- eTlong[wave == 1, .(m_age = mean(age, na.rm = TRUE), sd_age = sd(age, na.rm = TRUE))]
eT1F[, `:=` (outcome = "age", 
             measure = "mean (sd)", 
             Overall = paste0(formatC(m_age, digits = 2, format = "f"), " (", formatC(sd_age, digits = 3, format = "f"),")"))]
eT1F <- eT1F[, .(outcome, measure, Overall)]
eT1G <- eTlong[wave == 1, .(m_age = mean(age, na.rm = TRUE), sd_age = sd(age, na.rm = TRUE)), by = .(Randomization_Group)]
eT1G[, `:=` (age = paste0(formatC(m_age, digits = 2, format = "f"), "(", formatC(sd_age, digits = 3, format = "f"),")"))]
eT1G <- dcast(eT1G, . ~ Randomization_Group, value.var = "age")
setnames(eT1G, old = ".", new = "outcome")
eT1G[, `:=` (outcome = "age", measure = "mean (sd)")]
setcolorder(eT1G, "measure", after = "outcome")
eT1s <- eT1F[eT1G, on = .(outcome, measure)]



eT1lF <- rbindlist(lapply(catOutcomes, \(.x){
  setnames(eTlong[wave == 1,.N, by = c(.x)][, outcome := .x], old = .x, new = "measure")
}))
setcolorder(eT1lF, "outcome")
eT1lF[, p := 100*fifelse(is.na(measure), N/sum(N), N/ sum(N[!is.na(measure)])), by = .(outcome)
      ][, `:=` (Np = paste0(N, " (", formatC(p, digits = 2, format = "f"), "%)"),
                Randomization_Group = "Overall")]
setorder(eT1lF, outcome, measure)

eT1lG <- rbindlist(lapply(catOutcomes, \(.x){
  setnames(eTlong[wave == 1,.N, by = c(.x, "Randomization_Group")][, outcome := .x], old = .x, new = "measure")
}))
setcolorder(eT1lG, "outcome")
eT1lG[, p := 100*fifelse(is.na(measure), N/sum(N), N/ sum(N[!is.na(measure)])), by = .(outcome, Randomization_Group)
][, Np := paste0(N, " (", formatC(p, digits = 2, format = "f"), "%)")]
setorder(eT1lG, outcome, Randomization_Group, measure)

eT1l <- dcast(rbind(eT1lF,eT1lG)[, Randomization_Group := factor(Randomization_Group, levels = c("Overall", "Control", "Intervention"))], outcome + measure ~ Randomization_Group, value.var = c("Np"), fill = "0 (0.00%)")


characteristics <- rbind(Npw, eT1s, eT1l)
fwrite(characteristics, file = paste0(data_add, "../target/BcnMadCE/results/characteristicsT1.csv"))


## Primary/Secondary outcomes description ----------------------------------



intOutcomes <- c("phq_ads", "phq9", "gad7", "ptsd", "passc", "eq5d5l_6")
contOutcomes <- c(intOutcomes, grep("^(csri_sp|RES_E)", names(Tlong), value = TRUE))
c2dOutcomes <- c("phq9_depression", "gad7_anxiety")
catOutcomes <- c(c2dOutcomes, grep("^(m_T1_CSRI_SP|btq_\\d+$|covid19_)|(?i)^eq5d5l_[12345]", names(Tlong), value = TRUE))




eT2s <- eTlong[, unlist(lapply(.SD, \(.x) {
				     stpv <- tryCatch(shapiro.test(.x), error = function(e) ifelse(grepl("all 'x' values are identical", e), list(p.value = 1), e))$p.value
				     .x <- as.numeric(.x)
				     list(stpv0 = stpv,
					  stpv = fifelse(stpv < 0.05, 
							 paste0("Non normal: Shapiro-Wilk test p-value = ", formatC(stpv, digits = 3, flag = "#", format = "g")),
							 paste0("Normal: Shapiro-Wilk test p-value = ", formatC(stpv, digits = 3, format = "f"))),
					  nm = sum(!is.na(.x)),
					  min = min(.x, na.rm = TRUE),
					  max = max(.x, na.rm = TRUE),
					  mean = mean(.x, na.rm = TRUE),
					  sd = sd(.x, na.rm = TRUE),
					  lower = t.test(.x)$conf.int[1], 
					  upper = t.test(.x)$conf.int[2],
					  pval = t.test(.x)$p.value,
					  median = median(.x, na.rm = TRUE),
					  Q1 = quantile(.x, probs = 0.25, na.rm = TRUE),
					  Q3 = quantile(.x, probs = 0.75, na.rm = TRUE))}), 
		      recursive = FALSE), 
               by = .(wave, Randomization_Group),
	       .SDcols = contOutcomes]





(Tcont <- melt(eT2s, measure.vars = measure(outcome, value.name, pattern = "(^.*)\\.([a-zA-Z0-9]*)")))

cols <- c("mean", "lower", "upper")
Tcont[, (cols) := lapply(.SD, \(.x) formatC(.x, digits = 1, format = "f")), .SDcols = cols]
cols <- c("median", "Q1", "Q3", "sd")
Tcont[, (cols) := lapply(.SD, \(.x) formatC(.x, digits = 2, format = "f")), .SDcols = cols]

Tcont[, `:=` (`mean (sd) [CI]` = paste0(mean, " (", sd,") [", lower, ",", upper, "]"),
	      `median (Q1,Q3) [min,max]` = paste0(median, "(",
						  Q1, ",",
						  Q3, ") [",
						  fifelse(outcome %in% intOutcomes, formatC(min, format = "d"), formatC(min, digits = 2, format = "f")),
						  ",",
						  fifelse(outcome %in% intOutcomes, formatC(max, format = "d"), formatC(max, digits = 2, format = "f")),
						  "]"
						  ),
	   `number of subjects (n)` = formatC(nm, format = "d"),
	   `Shapiro-Wilk test p-value` = formatC(stpv0, digits = 3, format = "g"))]

Tcont <- dcast(melt(Tcont, id.vars = c("wave", "Randomization_Group", "outcome"), measure.vars = grep("\\(|-", names(Tcont)), variable.name = "measure"), outcome + measure ~ wave + Randomization_Group, value.var = "value")

setorder(Tcont, -outcome, measure)







eT2l <- rbindlist(lapply(catOutcomes, \(.x){
			 setnames(eTlong[,.N, by = c(.x, "wave", "Randomization_Group")][, outcome := .x], old = .x, new = "measure")
				     }))
setcolorder(eT2l, "outcome")
eT2l[, p := 100*fifelse(is.na(measure), N/sum(N), N/ sum(N[!is.na(measure)])), by = .(outcome, wave, Randomization_Group)
    ][, Np := paste0(N, " (", formatC(p, digits = 0, format = "f"), "%)")]
setorder(eT2l, outcome, Randomization_Group, wave, measure)
Tcat <- dcast(eT2l, outcome + measure ~ wave + Randomization_Group, value.var = c("Np"), fill = "0 (0.00%)")






NpwG <- dcast(eTlong[, .N, by = .(wave, Randomization_Group)], . ~ wave + Randomization_Group, value.var = "N")
setnames(NpwG, old = ".", new = "outcome")
NpwG$outcome <- NpwG$measure <- "N"
setcolorder(NpwG, "measure", after = "outcome")





NpwG <- rbind(NpwG, Tcont, Tcat)
setorder(NpwG, outcome, measure)
# formattable(Tcont,
# 	    list(area(row = 1:14, col = c("min", "max", "median")) ~ as.integer,
# 		 area(row = 1:14, col = c("Q1", "Q3", "mean", "sd")) ~ comma,
# 		 area(row = 15:17, col = min:Q3) ~ comma,
# 		 stpv = formatter("span", style = ~ style(color = fifelse(grepl("^Non normal", stpv), "red", "green")))))
# 
resultsT2 <- NpwG[(grepl("^(phq_ads|phq9|gad7|ptsd)$", outcome) & grepl("^mean", measure)) | outcome == "N" | grepl("^(phq9|gad7)_[a-z]+$", outcome)
                  ][c(1, 9, 6, 2, 10, 7, 8, 4, 5)]

fwrite(resultsT2, file = paste0(data_add, "../target/BcnMadCE/results/resultsT2.csv"))




## ITT linear mixed model -------------------------------------------------------------------

eTlong[, `:=` (baseline_phq_ads = phq_ads[wave == 1],
               time = factor(wave)), 
       by = .(Record_Id)]
fit0 <- lme(phq_ads ~ time + baseline_phq_ads, random = ~ 1 | Castor_Record_ID, data = Tlong, na.action = na.omit)
fit1 <- lme(phq_ads ~ time + baseline_phq_ads + time:Randomization_Group, random = ~ 1 | Record_Id, data = eTlong, na.action = na.omit)
fit11 <- lme(phq_ads ~ time + time*Randomization_Group, random = ~ 1 | Record_Id, data = eTlong, na.action = na.omit)
# summary(fit1)
# intervals(fit1)
# deltaMethod(fit1, "Randomization_GroupIntervention")
vcCR <- vcovCR(fit11, type = "CR2")
# coef_test(fit1, vcov = vcCR)
conf_int(fit11, vcov = vcCR)


# library(sandwich)
# library(merDeriv)
# library(lme4)
# fit14 <- lmer(phq_ads ~ time + time*Randomization_Group + (1 | Record_Id), data = eTlong)
# fixef(fit14)
# confint(fit14)
# sandwich(fit14)
# 
# library(robustlmm)
# rfit14 <- rlmer(phq_ads ~ time + time*Randomization_Group + (1 | Record_Id), data = eTlong)
# summary(rfit14)
# fixef(rfit14)


