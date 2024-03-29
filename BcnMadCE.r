# BcnMadCE.r
# from BcnMadCEdata_wranfactors.r

# OS dependencies ----------------------------------------------------------


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


# library(marginaleffects)
# library(modelsummary)
library(ggplot2)
library(ggrepel)
# library(performance)
# library(formattable)
# library(compareGroups)
# library(car)
# library(systemfit)
# library(lmtest) # coefci
library(emmeans)
# library(scdhlm)
# library(sandwich) # https://sandwich.r-forge.r-project.org/articles/sandwich.html
# library(merDeriv) # estfun.lmerMod, bread.lmerMod # https://stackoverflow.com/a/70268157/997979
# library(robustlmm) # rlmer
# source("https://raw.githubusercontent.com/masonFG/CIrobustLMM/master/CI_functions/confintLMMFast.R")
# source("https://raw.githubusercontent.com/masonFG/CIrobustLMM/master/CI_functions/BCaVarCompRob.R")
# source("https://raw.githubusercontent.com/masonFG/CIrobustLMM/master/CI_functions/BCaDAStau.R")
# source("https://raw.githubusercontent.com/masonFG/CIrobustLMM/master/CI_functions/BCaML.R")
library(glmmTMB)
# library(trouBBlme4SolveR)
library(lme4)
library(openxlsx)
library(multcomp)
library(clubSandwich)
library(nlme)
library(matrixStats)
library(data.table, lib.loc = dev_lib_path)






# Data loading --------------------------------------------------------------

load(paste0(data_path, "../target/BcnMadCE/CEdata2.rdata"))



# Data preparation  ----------------------------------------------------------------

Tlong[, `:=` (phq9_depression = factor(phq9 < 10, levels = c(FALSE, TRUE), labels = c("PHQ-9 >= 10", "PHQ-9 < 10")), gad7_anxiety = factor(gad7 < 10, levels = c(FALSE, TRUE), labels = c("GAD7 >= 10", "GAD7 < 10")))]
eTlong <- merge(screening, Tlong, all = TRUE, by.x = "Record_Id", by.y = "Castor_Record_ID")

eTlong[, `:=` (age = year(Survey_Completed_On) - t0_soc_02, 
	       to_soc_covid19 = fifelse(covid19_1 == "No", FALSE, TRUE), 
	       t0_soc_site = fifelse(Institute_Abbreviation == "SJD", "Bcn", "Mad"))
       ][, `:=` (baseline_phq_ads = phq_ads[wave == 1],
		 time = factor(wave, levels = 1:4, labels = c("T1. Baseline", "T2. Week 7 (post DWM)", "T3. Week 13 (post PM+)", "T4. Week 21 (follow-up)"))), 
       by = .(Record_Id)]

fwrite(eTlong, file = paste0(data_path, "../target/BcnMadCE/CEdata.csv"))



wb <- createWorkbook() 


# Data view ---------------------------------------------------------------



sheetDT <- eTlong[, aux := NULL][, aux := phq_ads[wave==4], by = .(Record_Id)][, aux := is.na(aux)][wave==1, .N, by = .(aux, t0_soc_01)][, p := round(100* N/sum(N), 2), by = .(t0_soc_01)][aux == TRUE]
sheetDT[, `:=` (variable = "phq_ads", time = "T4", aux = NULL)]
setcolorder(sheetDT, neworder = c("variable", "time"), before = "N")
addWorksheet(wb, sheetName = "Missingness by gender")
writeData(wb, sheet = "Missingness by gender", sheetDT)

sheetDT <- eTlong[, aux := NULL][, aux := phq_ads[wave==4], by = .(Record_Id)][, aux := is.na(aux)][wave==1, .N, by = .(aux, t0_soc_16)][, p := round(100* N/sum(N), 2), by = .(t0_soc_16)][aux == TRUE]
sheetDT[, `:=` (variable = "phq_ads", time = "T4", aux = NULL)]
setcolorder(sheetDT, neworder = c("variable", "time"), before = "N")
addWorksheet(wb, sheetName = "Missingness by type of job")
writeData(wb, sheet = "Missingness by type of job", sheetDT)

eTlong[, aux := NULL
       ][, aux := as.integer(difftime(Survey_Completed_On, Survey_Completed_On[wave==1], units = "days")), by = .(Record_Id)
       ][, .(median(aux)), by = .(Randomization_Group, Institute_Abbreviation, wave)
       ][, V2 := V1[Randomization_Group == "Intervention"] - V1[Randomization_Group == "Control"], by = .(Institute_Abbreviation, wave)
       ][wave != 1
       ][order(Institute_Abbreviation, wave, Randomization_Group)]

sheetDT <- eTlong[sbs_1 == "Sí"][, .(Record_Id, Institute_Abbreviation, wave)]
addWorksheet(wb, sheetName = "Adverse events")
writeData(wb, sheet = "Adverse events", sheetDT)

eTlong[, .N, by = .(wave, Institute_Abbreviation, Randomization_Group)
       ][, miss := N[wave == 1] - N, by = .(Institute_Abbreviation, Randomization_Group)
       ][, p := miss/N[wave == 1]*100, by = .(Institute_Abbreviation, Randomization_Group)
       ][wave != 1
       ][order(wave, -Institute_Abbreviation, -Randomization_Group)]

rbindlist(lapply(2:4, \(.wave) 
		 eTlong[, aux := NULL
			][, aux := phq_ads[wave == .wave], by = .(Record_Id)
			][, aux := is.na(aux)
			][wave==1, .N, by = .(aux, Institute_Abbreviation, Randomization_Group)
			][, p := 100* N/sum(N), by = .(Institute_Abbreviation, Randomization_Group)
			][aux == TRUE
			][order(-Institute_Abbreviation, -Randomization_Group)]))

sheetDT <- eTlong[, .(miss = sum(is.na(phq_ads)), .N), by = .(wave, time, Institute_Abbreviation, Randomization_Group)
		  ][, `:=` (miss = N[wave == 1] - N + miss, p = round((N[wave == 1] - N + miss)*100/N[wave == 1], 2)), by = .(Institute_Abbreviation, Randomization_Group)
		  ][wave != 1
		  ][order(wave, -Institute_Abbreviation, -Randomization_Group)
		  ][, !c("wave")]
addWorksheet(wb, sheetName = "Retention rates")
writeData(wb, sheet = "Retention rates", sheetDT)

# eTlong[wave == 1 & Institute_Abbreviation == "SJD" & Randomization_Group == "Control" & !Record_Id %in% eTlong[wave == 3]$Record_Id]$Record_Id

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



catOutcomes <- c("t0_soc_01", "t0_soc_12", "t0_soc_16", "t0_soc_17", "t0_soc_18", "to_soc_covid19", "t0_soc_site")


eT1F <- eTlong[wave == 1, .(m_age = mean(age, na.rm = TRUE), sd_age = sd(age, na.rm = TRUE))]
eT1F[, `:=` (outcome = "age", 
             measure = "mean (sd)", 
             Overall = paste0(formatC(m_age, digits = 1, format = "f"), " (", formatC(sd_age, digits = 2, format = "f"),")"))]
eT1F <- eT1F[, .(outcome, measure, Overall)]
eT1G <- eTlong[wave == 1, .(m_age = mean(age, na.rm = TRUE), sd_age = sd(age, na.rm = TRUE)), by = .(Randomization_Group)]
eT1G[, `:=` (age = paste0(formatC(m_age, digits = 1, format = "f"), " (", formatC(sd_age, digits = 2, format = "f"),")"))]
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


sheetDT <- characteristics <- rbind(Npw, eT1s, eT1l)
fwrite(characteristics, file = paste0(data_path, "../target/BcnMadCE/results/characteristicsT1.csv"))

addWorksheet(wb, sheetName = "Table 1")
writeData(wb, sheet = "Table 1", sheetDT)

## Primary/Secondary outcomes description ----------------------------------



intOutcomes <- c("phq_ads", "phq9", "gad7", "ptsd", "passc", "eq5d5l_6")
contOutcomes <- c(intOutcomes, grep("^(csri_sp|RES_E|EuroQoL_index)", names(Tlong), value = TRUE))
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
					  miss = sum(is.na(.x)),
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
               by = .(wave, time, Randomization_Group),
	       .SDcols = contOutcomes]





Tcontpre <- melt(eT2s, measure.vars = measure(outcome, value.name, pattern = "(^.*)\\.([a-zA-Z0-9]*)"))
Tcont <- Tcontpre[, !c("time")]

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
	   `Missing` = formatC(miss, format = "d"),
	   `Shapiro-Wilk test p-value` = formatC(stpv0, digits = 3, format = "g"))]

Tcont <- dcast(melt(Tcont, id.vars = c("wave", "Randomization_Group", "outcome"), measure.vars = grep("\\(|-|Missing", names(Tcont)), variable.name = "measure"), outcome + measure ~ wave + Randomization_Group, value.var = "value")

setorder(Tcont, -outcome, measure)







eT2l <- rbindlist(lapply(catOutcomes, \(.x){
			 setnames(eTlong[,.N, by = c(.x, "wave", "Randomization_Group")][, outcome := .x], old = .x, new = "measure")
				     }))
setcolorder(eT2l, "outcome")
eT2l[, p := 100*fifelse(is.na(measure), N/sum(N), N/ sum(N[!is.na(measure)])), by = .(outcome, wave, Randomization_Group)
     ][, Np := paste0(N, " (", formatC(p, digits = 0, format = "f"), "%)")]
setorder(eT2l, outcome, Randomization_Group, wave, measure)
Tcat <- dcast(eT2l, outcome + measure ~ wave + Randomization_Group, value.var = c("Np"), fill = "0 (0.00%)")
cols <- grep("outcome|measure", names(Tcat), invert = TRUE, value = TRUE)
Tcat[, `:=` (measure = factor(fifelse(is.na(measure), "Missing", as.character(measure)), levels = c(levels(Tcat$measure), "Missing")))][, (cols) := lapply(.SD, \(.x) fifelse(measure == "Missing", sub(" \\(.*$", "", .x), .x)), .SDcols = cols]





NpwG <- dcast(melt(eTlong[, .N, by = .(wave, Randomization_Group)][, Missing := N[wave == 1] - N, by = .(Randomization_Group)][], measure.vars = c("N", "Missing"), variable.name = "measure"), measure ~ wave + Randomization_Group, value.var = "value")
NpwG[, outcome := measure]
setcolorder(NpwG, "outcome")

misscont <- Tcont[, unique(.SD[measure == "Missing", .(outcome, measure)])]
misscat <- Tcat[, unique(.SD[measure == "Missing", .(outcome, measure)])]
misscat <- rbind(misscat, data.table(outcome = "phq9_depression", measure = "Missing"))
NpwG <- NpwG[c(1, 2, rep(2, nrow(misscont)), rep(2, nrow(misscat)))][3:(2 + nrow(misscont)), outcome := misscont$outcome][(3 + nrow(misscont)):(2 + nrow(misscont) + nrow(misscat)), outcome := misscat$outcome][]


# NpwG <- dcast(eTlong[, .N, by = .(wave, Randomization_Group)], . ~ wave + Randomization_Group, value.var = "N")
# setnames(NpwG, old = ".", new = "outcome")
# NpwG$outcome <- NpwG$measure <- "N"
# setcolorder(NpwG, "measure", after = "outcome")





NpwG <- rbind(NpwG, Tcont, Tcat)
cols <- grep("outcome|measure", names(NpwG), invert = TRUE, value = TRUE)
NpwG[measure == "Missing", (cols) := lapply(.SD, \(.x) as.character(sum(as.integer(.x)))), by = .(outcome, measure), .SDcols = cols]
NpwG <- NpwG[, unique(.SD)]
setorder(NpwG, outcome, measure)
# formattable(Tcont,
# 	    list(area(row = 1:14, col = c("min", "max", "median")) ~ as.integer,
# 		 area(row = 1:14, col = c("Q1", "Q3", "mean", "sd")) ~ comma,
# 		 area(row = 15:17, col = min:Q3) ~ comma,
# 		 stpv = formatter("span", style = ~ style(color = fifelse(grepl("^Non normal", stpv), "red", "green")))))
# 
sheetDT <- resultsT2 <- NpwG[(grepl("^(phq_ads|phq9|gad7|ptsd)$", outcome) & grepl("^(mean|Missing)", measure)) | outcome == "N" | grepl("^(phq9|gad7)_[a-z]+$", outcome)
                  ][c(1, 5, 4, 7, 6, 9, 8, 3, 2, 14, 15, 13, 11, 12, 10)]

fwrite(resultsT2, file = paste0(data_path, "../target/BcnMadCE/results/resultsT2.csv"))
fwrite(NpwG, file = paste0(data_path, "../target/BcnMadCE/results/descriptives.csv"))


addWorksheet(wb, sheetName = "Table 2")
writeData(wb, sheet = "Table 2", sheetDT)

saveWorkbook(wb, paste0(data_path, "../target/BcnMadCE/results/report.xlsx"), overwrite = TRUE)

## ITT linear mixed model -------------------------------------------------------------------

wb <- loadWorkbook(paste0(data_path, "../target/BcnMadCE/results/report.xlsx"))

### First tests -------------------------------------------------------------



fit0 <- lme(phq_ads ~ time + baseline_phq_ads, random = ~ 1 | Record_Id, data = eTlong, na.action = na.omit)
fit01 <- lme(phq_ads ~ time, random = ~ 1 | Record_Id, data = eTlong, na.action = na.omit)
fit1 <- lme(phq_ads ~ time + baseline_phq_ads + time:Randomization_Group, random = ~ 1 | Record_Id, data = eTlong, na.action = na.omit)

# summary(fit1)
# intervals(fit1) # nlme
# vcCR <- vcovCR(fit1, type = "CR2") # clubSandwich
# coef_test(fit1, vcov = vcCR) # clubSandwich
# conf_int(fit1, vcov = vcCR) # clubSandwich


### Robust reproducibility tests -------------------------------------------------------------

# nlme::lme
fit11 <- lme(phq_ads ~ Randomization_Group + time*Randomization_Group, random = ~ 1 | Record_Id, data = eTlong, na.action = na.omit)
# fit11 <- lme(phq_ads ~ time + time:Randomization_Group, random = ~ 1 | Record_Id, data = eTlong, na.action = na.omit) # the same
# summary(fit11)
# intervals(fit11) # intervals(fit11, which = "fixed")[["fixed"]][-1,]
vcCR11 <- vcovCR(fit11, type = "CR2")
# conf_int(fit11, vcov = vcCR11)

# lme4::lmer
fit14 <- lmer(phq_ads ~ Randomization_Group + time*Randomization_Group + (1 | Record_Id), data = eTlong)
# fit14 <- lmer(phq_ads ~ time + time:Randomization_Group + (1 | Record_Id), data = eTlong) # the same
# summary(fit14)
# cbind(fixef(fit14), confint(fit14)[-c(1,2),])
# sandwich(fit14) # sandwich, merDeriv # NOT working!!!
vcCR14 <- vcovCR(fit14, type = "CR2")
# conf_int(fit14, vcov = vcCR14)

# robustlmm::rlmer
# rfit14 <- rlmer(phq_ads ~ Randomization_Group + time*Randomization_Group + (1 | Record_Id), data = eTlong) # robustlmm
# summary(rfit14)
# fixef(rfit14)
# rCI <- BCaboot(model = rfit14, data = eTlong, clusterid = eTlong$Record_Id, methodCI = "wild", B = 10, confint.level = .95, BCa = TRUE) # NOT WORKING!!!



### (Non-mixed-effects) Linear models reproducibility tests -------------------------------------------------------------

# fit11 <- lm(phq_ads ~ time*Randomization_Group, data = eTlong, na.action = na.omit)
# cbind(coef(fit11), confint(fit11))
# vcHC <- vcovHC(fit11, type = "HC")
# cbind(coef(fit11), coefci(fit11, vcov = vcHC)) # lmtest



### Contrasts/effect sizes analyses -------------------------------------------------------------
set.seed(20221014)
fit11RG <- emmeans(fit11, "Randomization_Group", by = "time", vcov. = vcCR11, mode = "appx-satterthwaite")
fit11RG.pw <- emmeans(fit11, pairwise ~ Randomization_Group, by = "time", vcov = vcCR11, mode = "appx-satterthwaite")
emmip(fit11RG.pw$emmeans, Randomization_Group ~ time, CIs = TRUE)
eff_size(fit11RG, sigma = sigma(fit11), edf = min(summary(pairs(fit11RG))$df)) 

fit14RG <- emmeans(fit14, "Randomization_Group", by = "time", vcov. = vcCR14, lmer.df = "satterthwaite")
fit14RG.pw <- emmeans(fit14, pairwise ~ Randomization_Group, by = "time", vcov = vcCR14, lmer.df = "satterthwaite")
emmip(fit14RG.pw$emmeans, Randomization_Group ~ time, CIs = TRUE)
eff_size(fit14RG, sigma = sigma(fit14), edf = min(summary(pairs(fit14RG))$df)) 


# K <- cbind(matrix(0, 4, 4), diag(4))
# rownames(K) <- paste0("Intervention - Control | time", 1:4)
# colnames(K) <- names(coef(fit11))
# confint(glht(fit11, linfct = K))
# fit11RG <- emmeans(fit11, "Randomization_Group", by = "time")
# pairs(fit11RG) # extend glht results
# eff_size(fit11RG, sigma = sigma(fit14), edf = 434) # does not work
# comparisons(fit11, variables = list(Randomization_Group = "pairwise"), by = "time") # extend glht results

#### ITT -------------------------------------------------------------

models <- sapply(c("phq_ads", "phq9", "gad7", "ptsd"), \(.x) lme(as.formula(paste0(.x," ~ Randomization_Group + time*Randomization_Group")), random = ~ 1 | Record_Id, data = eTlong, na.action = na.omit), simplify = FALSE, USE.NAMES = TRUE)

sheetDT <- rbindlist(lapply(names(models), \(.x) { 
				    ntv <- intervals(models[[.x]], which = "fixed")[["fixed"]]
				    do.call(cbind, list(data.table(outcome = .x), term = rownames(ntv), ntv)) 
				     }))

sheetDT <- sheetDT[, lapply(.SD, formatC, format = "f", digits = 2), .SDcols = is.numeric, by = .(outcome, term)]
setcolorder(sheetDT, "est.", before = "lower")
# models <- lapply(c("phq_ads", "phq9", "gad7", "ptsd"), \(.x) lme(as.formula(paste0(.x," ~ Randomization_Group + time*Randomization_Group")), random = ~ 1 | Record_Id, data = eTlong, na.action = na.omit))
# modelsummary(models = models[[1]], estimate = "{estimate} ({conf.low}, {conf.high})", statistic = NULL, gof_map = "nobs")

addWorksheet(wb, sheetName = "Models (intention-to-treat)")
writeData(wb, sheet = "Models (intention-to-treat)", sheetDT)




esCD <- rbindlist(lapply(c("phq_ads", "phq9", "gad7", "ptsd"), \(.x) 
			 eTlong[wave != 1, 
				unlist(.(outcome = .x, (do.call(what = effectsize::cohens_d, args = list(x = reformulate("Randomization_Group", response = .x), data = na.omit(.SD, cols = c("Randomization_Group", .x)))))), 
				       recursive = FALSE), 
				by = .(time)
				][, lapply(.SD, formatC, digits = 2, format = "f"), .SDcols = c("Cohens_d", "CI_low", "CI_high"), by = .(time, outcome)
				][, Cohens_d := paste0(Cohens_d, " (", CI_low,",",CI_high,")")]))[, .(time, outcome, Cohens_d)]

esOR <- rbindlist(lapply(c("phq9_depression", "gad7_anxiety"), \(.x)  
			 eTlong[wave != 1, 
				unlist(.(outcome = sub("_[a-z]+$", "", .x), (do.call(what = effectsize::oddsratio, args = na.omit(.SD[, .(x = Randomization_Group, y = get(.x))])))), 
				       recursive = FALSE), 
				by = .(time)
				][, lapply(.SD, formatC, digits = 2, format = "f"), .SDcols = c("Odds_ratio", "CI_low", "CI_high"), by = .(time, outcome)
				][, Odds_ratio := paste0(Odds_ratio, " (", CI_low,",",CI_high,")")]))[, .(time, outcome, Odds_ratio)]

esLOR <- rbindlist(lapply(c("phq9_depression", "gad7_anxiety"), \(.x)  
			  eTlong[wave != 1, 
				 unlist(.(outcome = sub("_[a-z]+$", "", .x), (do.call(what = \(x, y) effectsize::oddsratio(x, y, log = TRUE), args = na.omit(.SD[, .(x = Randomization_Group, y = get(.x))])))), 
					recursive = FALSE), 
				 by = .(time)
				 ][, lapply(.SD, formatC, digits = 2, format = "f"), .SDcols = c("log_Odds_ratio", "CI_low", "CI_high"), by = .(time, outcome)
				 ][, log_Odds_ratio := paste0(log_Odds_ratio, " (", CI_low,",",CI_high,")")]))[, .(time, outcome, log_Odds_ratio)]

sheetDT <- effS <- Reduce(\(x, y) merge(x, y, all = TRUE), list(esCD, esOR, esLOR))[order(time, outcome)]


addWorksheet(wb, sheetName = "Effect sizes (ITT)")
writeData(wb, sheet = "Effect sizes (ITT)", sheetDT)


#### Per-Protocol PP -------------------------------------------------------------


eTlong[Randomization_Group == "Intervention" & wave == 2, step_up := +(k10_score >= 16)][, step_up := step_up[wave == 2], by = .(Record_Id)]

sheetDT <- eTlong[Randomization_Group == "Control" | (dwmN >= 3 & ((pmN >= 4 & step_up == 1) | step_up == 0))][, unique(.SD[, .(Record_Id, Randomization_Group)])][, .N, by = .(Randomization_Group)]
addWorksheet(wb, sheetName = "Participants in PP analysis")
writeData(wb, sheet = "Participants in PP analysis", sheetDT)

eTlongPP <- eTlong[Randomization_Group == "Control" | (dwmN >= 3 & ((pmN >= 4 & step_up == 1) | step_up == 0))]




models <- sapply(c("phq_ads", "phq9", "gad7", "ptsd"), \(.x) lme(as.formula(paste0(.x," ~ Randomization_Group + time*Randomization_Group")), random = ~ 1 | Record_Id, data = eTlongPP, na.action = na.omit), simplify = FALSE, USE.NAMES = TRUE)

sheetDT <- rbindlist(lapply(names(models), \(.x) { 
				    ntv <- intervals(models[[.x]], which = "fixed")[["fixed"]]
				    do.call(cbind, list(data.table(outcome = .x), term = rownames(ntv), ntv)) 
				     }))

sheetDT <- sheetDT[, lapply(.SD, formatC, format = "f", digits = 2), .SDcols = is.numeric, by = .(outcome, term)]
setcolorder(sheetDT, "est.", before = "lower")
addWorksheet(wb, sheetName = "Models (per protocol)")
writeData(wb, sheet = "Models (per protocol)", sheetDT)




esCD <- rbindlist(lapply(c("phq_ads", "phq9", "gad7", "ptsd"), \(.x) 
			 eTlongPP[wave != 1, 
				  unlist(.(outcome = .x, (do.call(what = effectsize::hedges_g, args = list(x = reformulate("Randomization_Group", response = .x), data = na.omit(.SD, cols = c("Randomization_Group", .x)))))), 
					 recursive = FALSE), 
				  by = .(wave)
				  ][, lapply(.SD, formatC, digits = 2, format = "f"), .SDcols = c("Hedges_g", "CI_low", "CI_high"), by = .(wave, outcome)
				  ][, Hedges_g := paste0(Hedges_g, " (", CI_low,",",CI_high,")")]))[, .(wave, outcome, Hedges_g)]

esOR <- rbindlist(lapply(c("phq9_depression", "gad7_anxiety"), \(.x)  
			 eTlongPP[wave != 1, 
				  unlist(.(outcome = sub("_[a-z]+$", "", .x), (do.call(what = effectsize::oddsratio, args = na.omit(.SD[, .(x = Randomization_Group, y = get(.x))])))), 
					 recursive = FALSE), 
				  by = .(wave)
				  ][, lapply(.SD, formatC, digits = 2, format = "f"), .SDcols = c("Odds_ratio", "CI_low", "CI_high"), by = .(wave, outcome)
				  ][, Odds_ratio := paste0(Odds_ratio, " (", CI_low,",",CI_high,")")]))[, .(wave, outcome, Odds_ratio)]

esLOR <- rbindlist(lapply(c("phq9_depression", "gad7_anxiety"), \(.x)  
			  eTlongPP[wave != 1, 
				   unlist(.(outcome = sub("_[a-z]+$", "", .x), (do.call(what = \(x, y) effectsize::oddsratio(x, y, log = TRUE), args = na.omit(.SD[, .(x = Randomization_Group, y = get(.x))])))), 
					  recursive = FALSE), 
				   by = .(wave)
				   ][, lapply(.SD, formatC, digits = 2, format = "f"), .SDcols = c("log_Odds_ratio", "CI_low", "CI_high"), by = .(wave, outcome)
				   ][, log_Odds_ratio := paste0(log_Odds_ratio, " (", CI_low,",",CI_high,")")]))[, .(wave, outcome, log_Odds_ratio)]

sheetDT <- effS <- Reduce(\(x, y) merge(x, y, all = TRUE), list(esCD, esOR, esLOR))[order(wave, outcome)]

addWorksheet(wb, sheetName = "Effect sizes (PP)")
writeData(wb, sheet = "Effect sizes (PP)", sheetDT)

### Sensitivity analyses per site -------------------------------------------------------------
#### ITT -------------------------------------------------------------


models <- sapply(c("phq_ads", "phq9", "gad7", "ptsd"), \(.x) lme(as.formula(paste0(.x," ~ Randomization_Group + time*Randomization_Group")), random = list(Record_Id = ~ 1, Institute_Abbreviation = ~ 1), data = eTlong, na.action = na.omit), simplify = FALSE, USE.NAMES = TRUE)

sheetDT <- rbindlist(lapply(names(models), \(.x) { 
				    ntv <- intervals(models[[.x]], which = "fixed")[["fixed"]]
				    do.call(cbind, list(data.table(outcome = .x), term = rownames(ntv), ntv)) 
				     }))


sheetDT <- sheetDT[, lapply(.SD, formatC, format = "f", digits = 2), .SDcols = is.numeric, by = .(outcome, term)]
setcolorder(sheetDT, "est.", before = "lower")
addWorksheet(wb, sheetName = "Sensitivity - ITT nested")
writeData(wb, sheet = "Sensitivity - ITT nested", sheetDT)

#### Per-Protocol PP -------------------------------------------------------------

models <- sapply(c("phq_ads", "phq9", "gad7", "ptsd"), \(.x) lme(as.formula(paste0(.x," ~ Randomization_Group + time*Randomization_Group")), random = list(Record_Id = ~ 1, Institute_Abbreviation = ~ 1), data = eTlongPP, na.action = na.omit), simplify = FALSE, USE.NAMES = TRUE)

sheetDT <- rbindlist(lapply(names(models), \(.x) { 
				    ntv <- intervals(models[[.x]], which = "fixed")[["fixed"]]
				    do.call(cbind, list(data.table(outcome = .x), term = rownames(ntv), ntv)) 
				     }))
sheetDT <- sheetDT[, lapply(.SD, formatC, format = "f", digits = 2), .SDcols = is.numeric, by = .(outcome, term)]
setcolorder(sheetDT, "est.", before = "lower")
addWorksheet(wb, sheetName = "Sensitivity - PP nested")
writeData(wb, sheet = "Sensitivity - PP nested", sheetDT)

### Symptom severity sensitivity analyses -------------------------------------------------------------

eTlongSS <- eTlong[, aux := NULL][wave == 1, aux := phq_ads >= 20][, aux := aux[wave == 1], by = .(Record_Id)][aux == TRUE]
sheetDT <- unique(eTlongSS, by = c("Record_Id", "Randomization_Group"))[, .N, .(Randomization_Group)]
addWorksheet(wb, sheetName = "Participants with severity")
writeData(wb, sheet = "Participants with severity", sheetDT)


models <- sapply(c("phq_ads", "phq9", "gad7", "ptsd"), \(.x) lme(as.formula(paste0(.x," ~ Randomization_Group + time*Randomization_Group")), random = ~ 1 | Record_Id, data = eTlongSS, na.action = na.omit), simplify = FALSE, USE.NAMES = TRUE)

sheetDT <- rbindlist(lapply(names(models), \(.x) { 
				    ntv <- intervals(models[[.x]], which = "fixed")[["fixed"]]
				    do.call(cbind, list(data.table(outcome = .x), term = rownames(ntv), ntv)) 
				     }))
sheetDT <- sheetDT[, lapply(.SD, formatC, format = "f", digits = 2), .SDcols = is.numeric, by = .(outcome, term)]
setcolorder(sheetDT, "est.", before = "lower")
addWorksheet(wb, sheetName = "Sensitivity distressed")
writeData(wb, sheet = "Sensitivity distressed", sheetDT)

esCD <- rbindlist(lapply(c("phq_ads", "phq9", "gad7", "ptsd"), \(.x) 
			 eTlongSS[wave != 1, 
				  unlist(.(outcome = .x, (do.call(what = effectsize::hedges_g, args = list(x = reformulate("Randomization_Group", response = .x), data = na.omit(.SD, cols = c("Randomization_Group", .x)))))), 
					 recursive = FALSE), 
				  by = .(wave)
				  ][, lapply(.SD, formatC, digits = 2, format = "f"), .SDcols = c("Hedges_g", "CI_low", "CI_high"), by = .(wave, outcome)
				  ][, Hedges_g := paste0(Hedges_g, " (", CI_low,",",CI_high,")")]))[, .(wave, outcome, Hedges_g)]

esOR <- rbindlist(lapply(c("phq9_depression", "gad7_anxiety"), \(.x)  
			 eTlongSS[wave != 1, 
				  unlist(.(outcome = sub("_[a-z]+$", "", .x), (do.call(what = effectsize::oddsratio, args = na.omit(.SD[, .(x = Randomization_Group, y = get(.x))])))), 
					 recursive = FALSE), 
				  by = .(wave)
				  ][, lapply(.SD, formatC, digits = 2, format = "f"), .SDcols = c("Odds_ratio", "CI_low", "CI_high"), by = .(wave, outcome)
				  ][, Odds_ratio := paste0(Odds_ratio, " (", CI_low,",",CI_high,")")]))[, .(wave, outcome, Odds_ratio)]

esLOR <- rbindlist(lapply(c("phq9_depression", "gad7_anxiety"), \(.x)  
			  eTlongSS[wave != 1, 
				   unlist(.(outcome = sub("_[a-z]+$", "", .x), (do.call(what = \(x, y) effectsize::oddsratio(x, y, log = TRUE), args = na.omit(.SD[, .(x = Randomization_Group, y = get(.x))])))), 
					  recursive = FALSE), 
				   by = .(wave)
				   ][, lapply(.SD, formatC, digits = 2, format = "f"), .SDcols = c("log_Odds_ratio", "CI_low", "CI_high"), by = .(wave, outcome)
				   ][, log_Odds_ratio := paste0(log_Odds_ratio, " (", CI_low,",",CI_high,")")]))[, .(wave, outcome, log_Odds_ratio)]

sheetDT <- effS <- Reduce(\(x, y) merge(x, y, all = TRUE), list(esCD, esOR, esLOR))[order(wave, outcome)]

addWorksheet(wb, sheetName = "Effect sizes distressed")
writeData(wb, sheet = "Effect sizes distressed", sheetDT)


### Female sensitivity analyses -------------------------------------------------------------

eTlongFF <- eTlong[t0_soc_01 == "Mujer"]
sheetDT <- unique(eTlongFF[, .(Record_Id, Randomization_Group)])[, .N, by = .(Randomization_Group)]
addWorksheet(wb, sheetName = "Female participants")
writeData(wb, sheet = "Female participants", sheetDT)



models <- sapply(c("phq_ads", "phq9", "gad7", "ptsd"), \(.x) lme(as.formula(paste0(.x," ~ Randomization_Group + time*Randomization_Group")), random = ~ 1 | Record_Id, data = eTlongFF, na.action = na.omit), simplify = FALSE, USE.NAMES = TRUE)

sheetDT <- rbindlist(lapply(names(models), \(.x) { 
				    ntv <- intervals(models[[.x]], which = "fixed")[["fixed"]]
				    do.call(cbind, list(data.table(outcome = .x), term = rownames(ntv), ntv)) 
				     }))
sheetDT <- sheetDT[, lapply(.SD, formatC, format = "f", digits = 2), .SDcols = is.numeric, by = .(outcome, term)]
setcolorder(sheetDT, "est.", before = "lower")
addWorksheet(wb, sheetName = "Sensitivity - Female")
writeData(wb, sheet = "Sensitivity - Female", sheetDT)

esCD <- rbindlist(lapply(c("phq_ads", "phq9", "gad7", "ptsd"), \(.x) 
			 eTlongFF[wave != 1, 
				  unlist(.(outcome = .x, (do.call(what = effectsize::cohens_d, args = list(x = reformulate("Randomization_Group", response = .x), data = na.omit(.SD, cols = c("Randomization_Group", .x)))))), 
					 recursive = FALSE), 
				  by = .(wave)
				  ][, lapply(.SD, formatC, digits = 2, format = "f"), .SDcols = c("Cohens_d", "CI_low", "CI_high"), by = .(wave, outcome)
				  ][, Cohens_d := paste0(Cohens_d, " (", CI_low,",",CI_high,")")]))[, .(wave, outcome, Cohens_d)]

esOR <- rbindlist(lapply(c("phq9_depression", "gad7_anxiety"), \(.x)  
			 eTlongFF[wave != 1, 
				  unlist(.(outcome = sub("_[a-z]+$", "", .x), (do.call(what = effectsize::oddsratio, args = na.omit(.SD[, .(x = Randomization_Group, y = get(.x))])))), 
					 recursive = FALSE), 
				  by = .(wave)
				  ][, lapply(.SD, formatC, digits = 2, format = "f"), .SDcols = c("Odds_ratio", "CI_low", "CI_high"), by = .(wave, outcome)
				  ][, Odds_ratio := paste0(Odds_ratio, " (", CI_low,",",CI_high,")")]))[, .(wave, outcome, Odds_ratio)]

esLOR <- rbindlist(lapply(c("phq9_depression", "gad7_anxiety"), \(.x)  
			  eTlongFF[wave != 1, 
				   unlist(.(outcome = sub("_[a-z]+$", "", .x), (do.call(what = \(x, y) effectsize::oddsratio(x, y, log = TRUE), args = na.omit(.SD[, .(x = Randomization_Group, y = get(.x))])))), 
					  recursive = FALSE), 
				   by = .(wave)
				   ][, lapply(.SD, formatC, digits = 2, format = "f"), .SDcols = c("log_Odds_ratio", "CI_low", "CI_high"), by = .(wave, outcome)
				   ][, log_Odds_ratio := paste0(log_Odds_ratio, " (", CI_low,",",CI_high,")")]))[, .(wave, outcome, log_Odds_ratio)]

sheetDT <- effS <- Reduce(\(x, y) merge(x, y, all = TRUE), list(esCD, esOR, esLOR))[order(wave, outcome)]

addWorksheet(wb, sheetName = "Effect sizes - Female")
writeData(wb, sheet = "Effect sizes - Female", sheetDT)




### Adjusted estimates (incl. baseline covariate) sensitivity analyses -------------------------------------------------------------

cols <- c("t0_soc_18", "Institute_Abbreviation")
eTlong[, (cols) := lapply(.SD, \(.x) factor(.x, levels = levels(.x)[c(2,1)])), .SDcols = cols]

models <- sapply(c("phq_ads", "phq9", "gad7", "ptsd"), \(.x) lme(as.formula(paste0(.x," ~ Randomization_Group + time*Randomization_Group + t0_soc_01 + t0_soc_02 + t0_soc_12 + t0_soc_18 + Institute_Abbreviation + baseline_phq_ads")), random = ~ 1 | Record_Id, data = eTlong, na.action = na.omit), simplify = FALSE, USE.NAMES = TRUE)

sheetDT <- rbindlist(lapply(names(models), \(.x) { 
				    ntv <- intervals(models[[.x]], which = "fixed")[["fixed"]]
				    do.call(cbind, list(data.table(outcome = .x), term = rownames(ntv), ntv)) 
				     }))
sheetDT <- sheetDT[, lapply(.SD, formatC, format = "f", digits = 2), .SDcols = is.numeric, by = .(outcome, term)]
setcolorder(sheetDT, "est.", before = "lower")

addWorksheet(wb, sheetName = "Adjusted estimates")
writeData(wb, sheet = "Adjusted estimates", sheetDT)

# fit14 <- lmer(phq_ads ~ Randomization_Group + time*Randomization_Group + t0_soc_01 + t0_soc_02 + t0_soc_12 + t0_soc_18 + Institute_Abbreviation + baseline_phq_ads + (1 | Record_Id), data = eTlong)
# cbind(fixef(fit14), confint(fit14)[-c(1,2),])




### Complete case sensitivity analyses -------------------------------------------------------------




eTlongCC <- eTlong[!is.na(phq_ads) & !is.na(ptsd)][, aux := NULL][, aux := .N, by = .(Record_Id)][aux == 4]
sheetDT <- unique(eTlongCC[, .(Record_Id, Randomization_Group)])[, .N, by = .(Randomization_Group)]
addWorksheet(wb, sheetName = "Complete cases")
writeData(wb, sheet = "Complete cases", sheetDT)



models <- sapply(c("phq_ads", "phq9", "gad7", "ptsd"), \(.x) lme(as.formula(paste0(.x," ~ Randomization_Group + time*Randomization_Group")), random = ~ 1 | Record_Id, data = eTlongCC, na.action = na.omit), simplify = FALSE, USE.NAMES = TRUE)

sheetDT <- rbindlist(lapply(names(models), \(.x) { 
				    ntv <- intervals(models[[.x]], which = "fixed")[["fixed"]]
				    do.call(cbind, list(data.table(outcome = .x), term = rownames(ntv), ntv)) 
				     }))
sheetDT <- sheetDT[, lapply(.SD, formatC, format = "f", digits = 2), .SDcols = is.numeric, by = .(outcome, term)]
setcolorder(sheetDT, "est.", before = "lower")

addWorksheet(wb, sheetName = "Sensitivity - CC")
writeData(wb, sheet = "Sensitivity - CC", sheetDT)

### Sqrt outcomes sensitivity analyses -------------------------------------------------------------


models <- sapply(c("phq_ads", "phq9", "gad7", "ptsd"), \(.x) lme(as.formula(paste0("sqrt(", .x,") ~ Randomization_Group + time*Randomization_Group")), random = ~ 1 | Record_Id, data = eTlong, na.action = na.omit), simplify = FALSE, USE.NAMES = TRUE)

sheetDT <- rbindlist(lapply(names(models), \(.x) { 
				    ntv <- intervals(models[[.x]], which = "fixed")[["fixed"]]
				    do.call(cbind, list(data.table(outcome = .x), term = rownames(ntv), ntv)) 
				     }))

sheetDT <- sheetDT[, lapply(.SD, formatC, format = "f", digits = 2), .SDcols = is.numeric, by = .(outcome, term)]
setcolorder(sheetDT, "est.", before = "lower")

addWorksheet(wb, sheetName = "Sensitivity - SQRT")
writeData(wb, sheet = "Sensitivity - SQRT", sheetDT)


saveWorkbook(wb, paste0(data_path, "../target/BcnMadCE/results/report2.xlsx"), overwrite = TRUE)


### Binary outcomes (logistics models) sensitivity analyses -------------------------------------------------------------

wb <- loadWorkbook(paste0(data_path, "../target/BcnMadCE/results/report2.xlsx"))


# sheetDT <- rbindlist(lapply(c("phq9_depression", "gad7_anxiety"), \(.x) { 
# 				    fit <- dwmw(glmer(as.formula(paste0(.x, " ~ Randomization_Group + time*Randomization_Group + (1 | Record_Id)")), data = eTlong, family = binomial))
# 				    # exp(cbind(fit, confint(fit))) 
# 				     }))


models <- sapply(c("phq9_depression", "gad7_anxiety"), \(.x) glmmTMB(as.formula(paste0(.x, " ~ Randomization_Group + time*Randomization_Group + (1 | Record_Id)")), data = eTlong, family = binomial), simplify = FALSE, USE.NAMES = TRUE)

sheetDT <- rbindlist(lapply(names(models), \(.x) { 
				    ntv <- exp(confint(models[[.x]]))
				    # ntv <- ntv[!grepl("\\(Intercept\\)", rownames(ntv)),]
				    do.call(cbind, list(data.table(outcome = .x), term = rownames(ntv), ntv)) 
				     }))

sheetDT <- sheetDT[, lapply(.SD, formatC, format = "f", digits = 2), .SDcols = is.numeric, by = .(outcome, term)]
setcolorder(sheetDT, "Estimate", before = "2.5 %")

addWorksheet(wb, sheetName = "Binary outcomes")
writeData(wb, sheet = "Binary outcomes", sheetDT)



### Concurrent psychotherapy sensitivity analyses -------------------------------------------------------------

eTlongCP <- eTlong[, aux := NULL][, aux := .N, by = .(Record_Id)][(csri_sp_mental_g_n == 0 | is.na(csri_sp_mental_g_n)) & (csri_sp_mental_i_n == 0 | is.na(csri_sp_mental_i_n))][, aux2 := .N, by = .(Record_Id)][aux == aux2]
sheetDT <- unique(eTlongCP[, .(Record_Id, Randomization_Group)])[, .N, by = .(Randomization_Group)]
addWorksheet(wb, sheetName = "Concurrent psychotherapy")
writeData(wb, sheet = "Concurrent psychotherapy", sheetDT)



models <- sapply(c("phq_ads", "phq9", "gad7", "ptsd"), \(.x) lme(as.formula(paste0(.x," ~ Randomization_Group + time*Randomization_Group")), random = ~ 1 | Record_Id, data = eTlongCP, na.action = na.omit), simplify = FALSE, USE.NAMES = TRUE)

sheetDT <- rbindlist(lapply(names(models), \(.x) { 
				    ntv <- intervals(models[[.x]], which = "fixed")[["fixed"]]
				    do.call(cbind, list(data.table(outcome = .x), term = rownames(ntv), ntv)) 
				     }))
sheetDT <- sheetDT[, lapply(.SD, formatC, format = "f", digits = 2), .SDcols = is.numeric, by = .(outcome, term)]
setcolorder(sheetDT, "est.", before = "lower")
addWorksheet(wb, sheetName = "Sensitivity - CP")
writeData(wb, sheet = "Sensitivity - CP", sheetDT)

esCD <- rbindlist(lapply(c("phq_ads", "phq9", "gad7", "ptsd"), \(.x) 
			 eTlongCP[wave != 1, 
				  unlist(.(outcome = .x, (do.call(what = effectsize::hedges_g, args = list(x = reformulate("Randomization_Group", response = .x), data = na.omit(.SD, cols = c("Randomization_Group", .x)))))), 
					 recursive = FALSE), 
				  by = .(wave)
				  ][, lapply(.SD, formatC, digits = 2, format = "f"), .SDcols = c("Hedges_g", "CI_low", "CI_high"), by = .(wave, outcome)
				  ][, Hedges_g := paste0(Hedges_g, " (", CI_low,",",CI_high,")")]))[, .(wave, outcome, Hedges_g)]

esOR <- rbindlist(lapply(c("phq9_depression", "gad7_anxiety"), \(.x)  
			 eTlongCP[wave != 1, 
				  unlist(.(outcome = sub("_[a-z]+$", "", .x), (do.call(what = effectsize::oddsratio, args = na.omit(.SD[, .(x = Randomization_Group, y = get(.x))])))), 
					 recursive = FALSE), 
				  by = .(wave)
				  ][, lapply(.SD, formatC, digits = 2, format = "f"), .SDcols = c("Odds_ratio", "CI_low", "CI_high"), by = .(wave, outcome)
				  ][, Odds_ratio := paste0(Odds_ratio, " (", CI_low,",",CI_high,")")]))[, .(wave, outcome, Odds_ratio)]

esLOR <- rbindlist(lapply(c("phq9_depression", "gad7_anxiety"), \(.x)  
			  eTlongCP[wave != 1, 
				   unlist(.(outcome = sub("_[a-z]+$", "", .x), (do.call(what = \(x, y) effectsize::oddsratio(x, y, log = TRUE), args = na.omit(.SD[, .(x = Randomization_Group, y = get(.x))])))), 
					  recursive = FALSE), 
				   by = .(wave)
				   ][, lapply(.SD, formatC, digits = 2, format = "f"), .SDcols = c("log_Odds_ratio", "CI_low", "CI_high"), by = .(wave, outcome)
				   ][, log_Odds_ratio := paste0(log_Odds_ratio, " (", CI_low,",",CI_high,")")]))[, .(wave, outcome, log_Odds_ratio)]

sheetDT <- effS <- Reduce(\(x, y) merge(x, y, all = TRUE), list(esCD, esOR, esLOR))[order(wave, outcome)]

addWorksheet(wb, sheetName = "Effect sizes - CP")
writeData(wb, sheet = "Effect sizes - CP", sheetDT)





### Full programme sensitivity analyses -------------------------------------------------------------

eTlongFP <- eTlong[Randomization_Group == "Control" | (dwmN >= 3 & pmN >= 4)]
sheetDT <- eTlongFP[, unique(.SD[, .(Record_Id, Randomization_Group)])][, .N, by = .(Randomization_Group)]



addWorksheet(wb, sheetName = "Full program")
writeData(wb, sheet = "Full program", sheetDT)



models <- sapply(c("phq_ads", "phq9", "gad7", "ptsd"), \(.x) lme(as.formula(paste0(.x," ~ Randomization_Group + time*Randomization_Group")), random = ~ 1 | Record_Id, data = eTlongFP, na.action = na.omit), simplify = FALSE, USE.NAMES = TRUE)

sheetDT <- rbindlist(lapply(names(models), \(.x) { 
				    ntv <- intervals(models[[.x]], which = "fixed")[["fixed"]]
				    do.call(cbind, list(data.table(outcome = .x), term = rownames(ntv), ntv)) 
				     }))
sheetDT <- sheetDT[, lapply(.SD, formatC, format = "f", digits = 2), .SDcols = is.numeric, by = .(outcome, term)]
setcolorder(sheetDT, "est.", before = "lower")
addWorksheet(wb, sheetName = "Sensitivity - FP")
writeData(wb, sheet = "Sensitivity - FP", sheetDT)

esCD <- rbindlist(lapply(c("phq_ads", "phq9", "gad7", "ptsd"), \(.x) 
			 eTlongFP[wave != 1, 
				  unlist(.(outcome = .x, (do.call(what = effectsize::hedges_g, args = list(x = reformulate("Randomization_Group", response = .x), data = na.omit(.SD, cols = c("Randomization_Group", .x)))))), 
					 recursive = FALSE), 
				  by = .(wave)
				  ][, lapply(.SD, formatC, digits = 2, format = "f"), .SDcols = c("Hedges_g", "CI_low", "CI_high"), by = .(wave, outcome)
				  ][, Hedges_g := paste0(Hedges_g, " (", CI_low,",",CI_high,")")]))[, .(wave, outcome, Hedges_g)]

esOR <- rbindlist(lapply(c("phq9_depression", "gad7_anxiety"), \(.x)  
			 eTlongFP[wave != 1, 
				  unlist(.(outcome = sub("_[a-z]+$", "", .x), (do.call(what = effectsize::oddsratio, args = na.omit(.SD[, .(x = Randomization_Group, y = get(.x))])))), 
					 recursive = FALSE), 
				  by = .(wave)
				  ][, lapply(.SD, formatC, digits = 2, format = "f"), .SDcols = c("Odds_ratio", "CI_low", "CI_high"), by = .(wave, outcome)
				  ][, Odds_ratio := paste0(Odds_ratio, " (", CI_low,",",CI_high,")")]))[, .(wave, outcome, Odds_ratio)]

esLOR <- rbindlist(lapply(c("phq9_depression", "gad7_anxiety"), \(.x)  
			  eTlongFP[wave != 1, 
				   unlist(.(outcome = sub("_[a-z]+$", "", .x), (do.call(what = \(x, y) effectsize::oddsratio(x, y, log = TRUE), args = na.omit(.SD[, .(x = Randomization_Group, y = get(.x))])))), 
					  recursive = FALSE), 
				   by = .(wave)
				   ][, lapply(.SD, formatC, digits = 2, format = "f"), .SDcols = c("log_Odds_ratio", "CI_low", "CI_high"), by = .(wave, outcome)
				   ][, log_Odds_ratio := paste0(log_Odds_ratio, " (", CI_low,",",CI_high,")")]))[, .(wave, outcome, log_Odds_ratio)]

sheetDT <- effS <- Reduce(\(x, y) merge(x, y, all = TRUE), list(esCD, esOR, esLOR))[order(wave, outcome)]

addWorksheet(wb, sheetName = "Effect sizes - FP")
writeData(wb, sheet = "Effect sizes - FP", sheetDT)















sheetDT <- Tlong[wave==1, sapply(.SD, \(.y) as.numeric(.y) - 1), .SDcols = patterns("phq9_0")]|> psych::alpha()

addWorksheet(wb, sheetName = "Cronbach PHQ-9")
writeData(wb, sheet = "Cronbach PHQ-9", cbind(sheetDT[["total"]], structure(as.data.frame.list(sheetDT$feldt), names = paste0("Feldt - ", names(sheetDT$feldt)))))
addWorksheet(wb, sheetName = "PHQ-9 Reliab. item dropped")
writeData(wb, sheet = "PHQ-9 Reliab. item dropped", sheetDT[["alpha.drop"]], rowNames = TRUE)
addWorksheet(wb, sheetName = "PHQ-9 Item statistics")
writeData(wb, sheet = "PHQ-9 Item statistics", sheetDT[["item.stats"]], rowNames = TRUE)
addWorksheet(wb, sheetName = "PHQ-9 Non missing frequency")
writeData(wb, sheet = "PHQ-9 Non missing frequency", sheetDT[["response.freq"]], rowNames = TRUE)



sheetDT <- Tlong[wave==1, sapply(.SD, \(.y) as.numeric(.y) - 1), .SDcols = patterns("gad7_\\d")]|> psych::alpha()
addWorksheet(wb, sheetName = "Cronbach GAD7")
writeData(wb, sheet = "Cronbach GAD7", cbind(sheetDT[["total"]], structure(as.data.frame.list(sheetDT$feldt), names = paste0("Feldt - ", names(sheetDT$feldt)))))
addWorksheet(wb, sheetName = "GAD7 Reliab. item dropped")
writeData(wb, sheet = "GAD7 Reliab. item dropped", sheetDT[["alpha.drop"]], rowNames = TRUE)
addWorksheet(wb, sheetName = "GAD7 Item statistics")
writeData(wb, sheet = "GAD7 Item statistics", sheetDT[["item.stats"]], rowNames = TRUE)
addWorksheet(wb, sheetName = "GAD7 Non missing frequency")
writeData(wb, sheet = "GAD7 Non missing frequency", sheetDT[["response.freq"]], rowNames = TRUE)

sheetDT <- Tlong[wave==1, sapply(.SD, \(.y) as.numeric(.y) - 1), .SDcols = patterns("pcl5_\\d")]|> psych::alpha()
addWorksheet(wb, sheetName = "Cronbach PCL-5")
writeData(wb, sheet = "Cronbach PCL-5", cbind(sheetDT[["total"]], structure(as.data.frame.list(sheetDT$feldt), names = paste0("Feldt - ", names(sheetDT$feldt)))))
addWorksheet(wb, sheetName = "PCL-5 Reliab. item dropped")
writeData(wb, sheet = "PCL-5 Reliab. item dropped", sheetDT[["alpha.drop"]], rowNames = TRUE)
addWorksheet(wb, sheetName = "PCL-5 Item statistics")
writeData(wb, sheet = "PCL-5 Item statistics", sheetDT[["item.stats"]], rowNames = TRUE)
addWorksheet(wb, sheetName = "PCL-5 Non missing frequency")
writeData(wb, sheet = "PCL-5 Non missing frequency", sheetDT[["response.freq"]], rowNames = TRUE)

sheetDT <- Tlong[wave==1, sapply(.SD, \(.y) as.numeric(.y) - 1), .SDcols = patterns("phq9_0|gad7_\\d")]|> psych::alpha()
addWorksheet(wb, sheetName = "Cronbach PHQ-ADS")
writeData(wb, sheet = "Cronbach PHQ-ADS", cbind(sheetDT[["total"]], structure(as.data.frame.list(sheetDT$feldt), names = paste0("Feldt - ", names(sheetDT$feldt)))))
addWorksheet(wb, sheetName = "PHQ-ADS Reliab. item dropped")
writeData(wb, sheet = "PHQ-ADS Reliab. item dropped", sheetDT[["alpha.drop"]], rowNames = TRUE)
addWorksheet(wb, sheetName = "PHQ-ADS Item statistics")
writeData(wb, sheet = "PHQ-ADS Item statistics", sheetDT[["item.stats"]], rowNames = TRUE)
addWorksheet(wb, sheetName = "PHQ-ADS Non missing frequency")
writeData(wb, sheet = "PHQ-ADS Non missing frequency", sheetDT[["response.freq"]], rowNames = TRUE)


saveWorkbook(wb, paste0(data_path, "../target/BcnMadCE/results/report3.xlsx"), overwrite = TRUE)



# EuroQoL -----------------------------------------------------------------



p <- Tcontpre[outcome %in% c("EuroQoL_index", "eq5d5l_6")][, `:=` (Om = Q1 - 1.5*(Q3 - Q1), OM = Q3 + 1.5*(Q3 - Q1), outcome = factor(outcome), time = factor(time, labels = sub("^T\\d\\. ", "", levels(time))))] |> 
  ggplot(aes(x = time, y = mean, colour = Randomization_Group)) +
  geom_line(aes(group = Randomization_Group)) +
  geom_text_repel(aes(label = round(mean, 2)), vjust = 2, show.legend = FALSE) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  scale_y_continuous(limits = \(.x) c(ifelse(max(.x, na.rm = TRUE) > 1, 0, -0.5), 10^ceiling(log10(.x)))) +
  facet_grid(rows = vars(outcome), scales = "free") +
  labs(x = "Timepoint", y = "Mean score value", title = "Mean and 95% CI for EQ5D5L scales per group across waves") +
  theme(legend.position = "bottom")


ggdata <- na.omit(melt(eTlong[, .(EuroQoL_index, eq5d5l_6, time, Randomization_Group)], id.vars = c("time", "Randomization_Group")))

q <- ggplot(ggdata, aes(x = time, y = value, colour = Randomization_Group)) +
  geom_boxplot(width = 0.1) +
  geom_line(data = ggdata[, .(value = median(value, na.rm = TRUE)), by = .(time, variable, Randomization_Group)], aes(group = Randomization_Group)) +
  geom_text_repel(data = ggdata[, .(value = median(value, na.rm = TRUE)), by = .(time, variable, Randomization_Group)], aes(label = round(value, 2)), show.legend = FALSE) +
  scale_y_continuous(limits = \(.x) c(ifelse(max(.x, na.rm = TRUE) > 1, 0, -0.5), max(.x, na.rm = TRUE))) +
  facet_grid(rows = vars(variable), scales = "free") +
  labs(x = "Timepoint", y = "Median score value", title = "Boxplots for EQ5D5L scales per group across waves") +
  theme(legend.position = "bottom")

ggsave(paste0(data_path, "../../BcnMadDOCS/EQ5D5LmperTIMEandGROUP.png"), p, width = 20, height = 35, units = "cm")
ggsave(paste0(data_path, "../../BcnMadDOCS/EQ5D5LbperTIMEandGROUP.png"), q, width = 20, height = 35, units = "cm")
  

# QC 2x2 ------------------------------------------------------------------

t1 <- dcast(eTlong[Institute_Abbreviation == "SJD", .(Record_Id, covid19_1, wave)], Record_Id ~ wave, value.var = "covid19_1")[, table(`1`, `2`, useNA = "always")] |> addmargins()
t2 <- dcast(eTlong[Institute_Abbreviation == "SJD", .(Record_Id, covid19_1, wave)], Record_Id ~ wave, value.var = "covid19_1")[, table(`2`, `3`, useNA = "always")] |> addmargins()
t3 <- dcast(eTlong[Institute_Abbreviation == "SJD", .(Record_Id, covid19_1, wave)], Record_Id ~ wave, value.var = "covid19_1")[, table(`3`, `4`, useNA = "always")] |> addmargins()
