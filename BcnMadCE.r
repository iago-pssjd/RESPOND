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

load(paste0(data_add, "../target/BcnMadCE/CEdata.rdata"))



# Analysis ----------------------------------------------------------------



## Stressor Reactivity ----------------------------------------------------------------------

MIMIS <- grep("^(?!le).*LIR", enTlong, value = TRUE, perl = TRUE) # negative lookbehind
MIMIS <- sub("^([a-z_0-9]+) .*$", "\\1", MIMIS)

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

Elist <- list(LE = LE, GS = GS, CS = CS, PS = PS)

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

# count method for all (does have it sense???, having DS and LE distinct raiting scales)
# Tlong[, `:=` (EDS = rowSums2(as.matrix(.SD[, unlist(Elist[-match("LE", names(Elist))]), with = FALSE])),
#               EC = rowSums2(as.matrix(.SD[, unlist(Elist), with = FALSE])))]

# same without defining EC
Tlong[, EDS := rowSums2(as.matrix(.SD[, unlist(Elist[-match("LE", names(Elist))]), with = FALSE]))]

SsE <- c(paste0("E", names(Elist)), "EDS")

### Normal Stressor Reactivity ----------------------------------------------------------------------

normalf <- gaussian()
nsrdata <- Tlong[, lapply(.SD, mean, na.rm = TRUE), .SDcols = c(SsE, "phq_ads"), by = .(Castor_Record_ID)]

# Kalisch averaging method for EC (maybe should it be applied also to EDS here???)
nsrdata[, EC := rowMeans2(scale(as.matrix(.SD[, c("EDS", "ELE"), with = FALSE])))] 


SRform <- lapply(SsE, reformulate, response = "phq_ads")
nsr <- lapply(SRform, lm, data = nsrdata[, -c("Castor_Record_ID")])
names(nsr) <- SsE
invisible(lapply(SsE, \(.x) nsrdata[, (paste0(.x, "_phqads")) := predict(nsr[[.x]], nsrdata)]))



### Individual Stressor Reactivity score ----------------------------------------------------------------------

srTlong <- merge(Tlong, nsrdata, by = "Castor_Record_ID", all = TRUE, sort = FALSE, suffixes = c("", "_nsr"))


invisible(lapply(SsE, \(.x) srTlong[, (paste0("SR_", .x)) := sqrt(rowSums2(as.matrix((.SD[, .SD, .SDcols = !patterns(paste0(.x,"_"))] - .SD[, .SD, .SDcols = patterns(paste0(.x,"_"))])^2))), 
                                         .SDcols = patterns(paste0(.x,"|phq_ads$"))]))


## Primary/Secondary outcomes description ----------------------------------


outcomes <- c("phq_ads" = "phq9_0|gad7_", "phq9" = "phq9_0", "gad7" = "gad7_", "ptsd" = "pcl5_")
ST1 <- rbindlist(lapply(seq_along(outcomes), \(.x){
  out <- names(outcomes)[.x]
  Tlong[, .(outcome = out,
            avg = mean(.SD[[out]], na.rm = TRUE), 
            sd = sd(.SD[[out]], na.rm = TRUE),
            median = median(.SD[[out]], na.rm = TRUE),
            Q1 = quantile(.SD[[out]], probs = 0.25, na.rm = TRUE),
            Q3 = quantile(.SD[[out]], probs = 0.75, na.rm = TRUE),
            max = max(.SD[[out]], na.rm = TRUE),
            min = min(.SD[[out]], na.rm = TRUE), 
            lower = t.test(.SD[[out]])$conf.int[1], 
            upper = t.test(.SD[[out]])$conf.int[2],
            pval = t.test(.SD[[out]])$p.value,
            alpha = cronbachs_alpha(.SD[, -out, with = FALSE])), 
        by = .(wave), 
        .SDcols = patterns(paste0("^((",outcomes[.x],")\\d|",names(outcomes)[.x],"$)"))]
}))


melt(dcast(ST1, outcome ~ wave, value.var = names(ST1[,-c("outcome", "wave")])),
     measure.vars = measure(coef = \(.x) factor(.x, levels = names(ST1[,-c("outcome", "wave")])), 
                            value.name, pattern = "^([a-zA-Z0-9]+)_([1234])$"))[order(outcome, coef)]


### Similar per institution or group ----------------------------------------



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


