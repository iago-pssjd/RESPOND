# BcnMadCEdata_SR.r
# from BcnMadCEdata_wran.r
# to BcnMadCEdata_wranfactors.r

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

load(paste0(data_path, "../target/BcnMadCE/CEdata.rdata"))



# Analysis ----------------------------------------------------------------



## Stressor Reactivity ----------------------------------------------------------------------

DH <- grep("^(?!le).*LIR", enTlong, value = TRUE, perl = TRUE) # negative lookbehind
DH <- sub("^([a-z_0-9]+) .*$", "\\1", DH)

LE <- grep("^le.*LIR", enTlong, value = TRUE, perl = TRUE) #  general life events (n=3)
LE <- sub("^([a-z_0-9]+) .*$", "\\1", LE)

GS <- grep("^eg.*LIR", enTlong, value = TRUE, perl = TRUE) # general stressors (6 items)
GS <- sub("^([a-z_0-9]+) .*$", "\\1", GS)

CS <- grep("^ec.*LIR", enTlong, value = TRUE, perl = TRUE) # COVID-19-specific stressors (5 items)
CS <- sub("^([a-z_0-9]+) .*$", "\\1", CS)
# or?
# CS <- c("ec_1", "ec_2", "eh_1", "eh_2")

PS <- grep("^eh.*LIR", enTlong, value = TRUE, perl = TRUE) # population-specific stressors (n=4)
PS <- sub("^([a-z_0-9]+) .*$", "\\1", PS)

Elist <- list(LE = LE, DH = DH, GS = GS, CS = CS, PS = PS)

### Stressor Exposure ----------------------------------------------------------------------


invisible(lapply(seq_along(Elist), \(.x) Tlong[, (paste0("E", names(Elist)[.x])) := rowSums2(as.matrix(.SD[, Elist[[.x]], with = FALSE]))]))

# MIMIS ara Daily Stressors (DS): Exposure Daily Stressors
# a la Veer (Assessment of stressors, p. 3)
# Exposure Combined stressors
# Tlong[, EDS := rowMeans2(scale(as.matrix(.SD[, unlist(Elist[-match("LE", names(Elist))]), with = FALSE])))
#       ][, EC := rowMeans2(scale(as.matrix(.SD[, c("EDS", "ELE"), with = FALSE])))] 


# count method (a la Kalisch [Residualization-Based Calculation of Stressor Reactivity, p. 6]) combined with Veer averaging method
# Tlong[, EDS := rowSums2(as.matrix(.SD[, unlist(Elist[-match("LE", names(Elist))]), with = FALSE]))
#       ][, EC := rowMeans2(scale(as.matrix(.SD[, c("EDS", "ELE"), with = FALSE])))] 


# same without defining EC
# Tlong[, EDS := rowSums2(as.matrix(.SD[, unlist(Elist[-match("LE", names(Elist))]), with = FALSE]))]
# SsE <- c(paste0("E", names(Elist)), "EDS")
# two previous lines are commented by adding MIMIS to Elist, so EMIMIS = EDS
# and
# SsE <- paste0("E", names(Elist))

# Comparisons will be made between three different stressor exposure counts: 
# EMIMIS = daily stressors (general, COVID-19, population-specific), 
# ELE = life event count, 
# EC = and a combined score of daily stressors and life events (combined mean z-scores of stressor counts)
# Then
SsE <- c("EDH", "ELE")

# averaging a la Veer, following RESPOND Statistical Analysis Protocol, version 4.0
Tlong[, EC := rowMeans2(scale(as.matrix(.SD[, SsE, with = FALSE])))] 
SsE <- c(SsE, "EC")




### Normal Stressor Reactivity ----------------------------------------------------------------------

normalf <- gaussian()
# normal stressor reactivity is the regression line of average mental health problems against average stressor exposure across all time points
# not only sliding time windows consisting specifically of three time points (Kalisch et al., 2021, Time Courses of Stressor Reactivity) [wave <= 3]
nsrdata <- na.omit(Tlong[, lapply(.SD, mean, na.rm = TRUE), .SDcols = c(SsE, "phq_ads"), by = .(Castor_Record_ID)])

SRforml <- lapply(SsE, reformulate, response = "phq_ads")
sSRforml <- lapply(paste0("scale(",SsE,")"), reformulate, response = "scale(phq_ads)")
SRforml[[length(SRforml) + 1]] <- as.formula("phq_ads ~ EDH + ELE")
nsrl <- lapply(SRforml, lm, data = nsrdata[, -c("Castor_Record_ID")])
snsrl <- lapply(sSRforml, lm, data = nsrdata[, -c("Castor_Record_ID")])
# examine explained variance
do.call(what = \(...) anova(..., test = "F"), args = nsrl)
sapply(nsrl, \(.x) summary(.x)[["r.squared"]])

# SRformq <- lapply(paste0("poly(",SsE,", 2)"), reformulate, response = "phq_ads")
# SRformq[[length(SRformq) + 1]] <- as.formula("phq_ads ~ poly(EDH, 2) + poly(ELE, 2)")
# nsrq <- lapply(SRformq, lm, data = nsrdata[, -c("Castor_Record_ID")])
# do.call(what = \(...) anova(..., test = "F"), args = nsrq)
# sapply(nsrq, \(.x) summary(.x)[["r.squared"]])

# do.call(what = anova, args = c(nsrl,nsrq))


# If the combined score explains more variance in the PHQ-ADS, 
# it will be used for the subsequent computation of the SR score, 
# else the separate exposure scores will be used (resulting in two separate SR scores, 
# expressing reactivity to daily hassles and to life events, respectively)

# The daily hassles exposure score explains more variance in the PHQ-ADS, so two separate SR scores will be used.

nsrdata <- Tlong[, lapply(.SD, mean, na.rm = TRUE), .SDcols = c(SsE, "phq_ads"), by = .(Castor_Record_ID)]
nsr <- lapply(SRforml[-length(SRforml)], lm, data = nsrdata[, -c("Castor_Record_ID")])
names(nsr) <- SsE

### Individual Stressor Reactivity score ----------------------------------------------------------------------


# one’s individual SR score at any time point is the distance of an individual’s P score to the regression line (a subject’s residual)
# SR scores will be computed for each time point. 
# The inverse of the SR score is considered an approximative index of outcome-based resilience.
invisible(lapply(SsE, \(.x) Tlong[, (paste0("SR_", .x)) := phq_ads - predict(nsr[[.x]], Tlong)][, (paste0("RES_", .x)) := predict(nsr[[.x]], Tlong) - phq_ads]))

# If the combined score explains more variance in the PHQ-ADS, it will be used for the subsequent computation of the SR score, 
# else the separate exposure scores will be used (resulting in two separate SR scores, expressing reactivity to daily hassles and to life events, respectively).
# The inverse of the SR score is considered an approximative index of outcome-based resilience.





save(Tdata, MTdata, Tlong, enTlong, screening, eTlong, file = paste0(data_path, "../target/BcnMadCE/CEdataSR.rdata"))
