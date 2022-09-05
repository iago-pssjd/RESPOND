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

# Load libraries -----------------------------------------------------------
library(haven)
library(anytime)
library(data.table)
library(openxlsx)


# Load datasets -----------------------------------------------------------


db <-read_dta(paste0(data_add, "BBDD_FINAL.dta"))
setDT(db)
mobility20 <- fread(paste0(data_add, "2020_ES_Region_Mobility_Report.csv"), na.strings = c("NA", ""))
mobility21 <- fread(paste0(data_add, "2021_ES_Region_Mobility_Report.csv"), na.strings = c("NA", ""))
mobility22 <- fread(paste0(data_add, "2022_ES_Region_Mobility_Report.csv"), na.strings = c("NA", ""))
stringency <- fread(paste0(data_add, "stringency.csv"))
conversion <- read.xlsx(paste0(data_add, "conversió codis CCAA.xlsx"), cols = c(2,3))


# Arrange datasets --------------------------------------------------------


# mobility ----------------------------------------------------------------

mobility20 <- mobility20[!is.na(sub_region_1) & is.na(sub_region_2), !c("metro_area", "census_fips_code")]
mobility21 <- mobility21[!is.na(sub_region_1) & is.na(sub_region_2), !c("metro_area", "census_fips_code")]
mobility22 <- mobility22[!is.na(sub_region_1) & is.na(sub_region_2), !c("metro_area", "census_fips_code")]
mobility <- rbindlist(lapply(ls(pattern = "^mobility\\d+"), get))
# date "2020-02-15"

## stringency --------------------------------------------------------------

stringency <- stringency[CountryName == "Spain"][, Date := as.IDate(anydate(Date))]
# Date "20200101"

## conversion --------------------------------------------------------------


setDT(conversion)
conversion[, CCAA := factor(CCAA)]
levs <- levels(conversion$CCAA)
conversion[, CCAA := as.integer(CCAA)]
setnafill(conversion, type = "locf", cols = "CCAA")
conversion[, `:=` (CCAA = paste(factor(CCAA, seq_along(levs), levs)), CODI.VARIABLE.RESIDENCE_W1 = as.double(substr(CODI.VARIABLE.RESIDENCE_W1, 3, 5)))]



# main db -----------------------------------------------------------------


db <- conversion[db, on = .(CODI.VARIABLE.RESIDENCE_W1 == residence_w1)]
table(db$BASELINE_CurrentMonth)
db[, `:=` (date_w1 = as.IDate(paste(year_w1, BASELINE_CurrentMonth, BASELINE_CurrentDay, sep = "-")),
           date_w2 = as.IDate(fifelse(year_w2 == -93, NA_character_, paste(year_w2, CurrentMonth_w2, CurrentDay_w2, sep = "-"))),
           date_w3 = as.IDate(fifelse(year_w3 == -93, NA_character_, paste(year_w3, CurrentMonth_w3, CurrentDay_w3, sep = "-"))))]


# melt(db)