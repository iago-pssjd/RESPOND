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
library(bit64)
library(forcats)
library(haven) # using haven instead of readstata13, since it reads labels
library(anytime)
library(data.table) # under version at least 1.14.3
library(openxlsx)


# Load datasets -----------------------------------------------------------


db <- read_dta(paste0(data_add, "BBDD_FINAL.dta"))
mobility20 <- fread(paste0(data_add, "2020_ES_Region_Mobility_Report.csv"), na.strings = c("NA", ""))
mobility21 <- fread(paste0(data_add, "2021_ES_Region_Mobility_Report.csv"), na.strings = c("NA", ""))
mobility22 <- fread(paste0(data_add, "2022_ES_Region_Mobility_Report.csv"), na.strings = c("NA", ""))
stringency <- fread(paste0(data_add, "stringency.csv"), na.strings = c("NA", ""))
conversion <- read.xlsx(paste0(data_add, "conversió codis CCAA.xlsx"), cols = c(2,3))


# Arrange datasets --------------------------------------------------------


## mobility ----------------------------------------------------------------

mobility20 <- mobility20[!is.na(sub_region_1) & is.na(sub_region_2), !c("metro_area", "census_fips_code")]
mobility21 <- mobility21[!is.na(sub_region_1) & is.na(sub_region_2), !c("metro_area", "census_fips_code")]
mobility22 <- mobility22[!is.na(sub_region_1) & is.na(sub_region_2), !c("metro_area", "census_fips_code")]
mobility <- rbindlist(lapply(ls(pattern = "^mobility\\d+"), get))
# date "2020-02-15"

mobility <- mobility[, !c("country_region_code", "country_region", "sub_region_2", "iso_3166_2_code", "place_id")]


## stringency --------------------------------------------------------------

stringency <- stringency[CountryName == "Spain"
                         ][, Date := as.IDate(anydate(Date))
                           ][, !c("CountryName", "CountryCode", "RegionName", "RegionCode", "Jurisdiction")]
# Date "20200101"



## conversion --------------------------------------------------------------


setDT(conversion)
# https://github.com/Rdatatable/data.table/issues/3992
conversion[, CCAA := factor(CCAA)]
levs <- levels(conversion$CCAA)
conversion[, CCAA := as.integer(CCAA)]
setnafill(conversion, type = "locf", cols = "CCAA")
conversion[, `:=` (CCAA = paste(factor(CCAA, seq_along(levs), levs)))]



# main db -----------------------------------------------------------------

setDT(db)
# db <- db |> 
#   zap_label() |> 
#   zap_labels() |> 
#   zap_formats()

db[, residence_w1 := as.character(fct_recode(as_factor(residence_w1), "-93" = "missing"))]

## merge conversion with db --------------------------------------------


db <- conversion[db, on = .(CODI.VARIABLE.RESIDENCE_W1 == residence_w1)]
setnames(db, old = c("CODI.VARIABLE.RESIDENCE_W1"), new = c("residence_w1"))

## Build date --------------------------------------------------------------

set.seed(as.integer64(paste0(sapply(strsplit("MINDCOVID", "")[[1]], match, table = LETTERS), collapse = "")))
db[, `:=` (rid = sample.int(nrow(db)),
           date_w1 = as.IDate(paste(year_w1, BASELINE_CurrentMonth, BASELINE_CurrentDay, sep = "-")),
           date_w2 = as.IDate(fifelse(year_w2 == -93, NA_character_, paste(year_w2, CurrentMonth_w2, CurrentDay_w2, sep = "-"))),
           date_w3 = as.IDate(fifelse(year_w3 == -93, NA_character_, paste(year_w3, CurrentMonth_w3, CurrentDay_w3, sep = "-"))))]
db <- db[, !c("BASELINE_CurrentMonth", "BASELINE_CurrentDay", "year_w1", 
        "CurrentMonth_w2", "CurrentDay_w2", "year_w2", 
        "CurrentMonth_w3", "CurrentDay_w3", "year_w3", 
        "Respondent_Serial_w2", "Respondent_Serial_w3",
        "Respondent_Serial_OLA2_w3")]

# grep("_w[123]$", names(db), value = TRUE, invert = TRUE) |> length()
# grep("_w[123]$", names(db), value = TRUE, invert = FALSE) |> length()
# lapply(1:3, \(.item) sub(paste0("_w",.item),"",grep(paste0("_w",.item,"$"), names(db), value = TRUE, invert = FALSE))) |> 
#   Reduce(f=union) |> length()


## Reshape wide to long ----------------------------------------------------


dbl <- melt(db, 
     # id.vars = c("BASELINE_Respondent_Serial", "CODI.VARIABLE.RESIDENCE_W1", "CCAA", "rid),
     measure.vars = measure(value.name, wave = as.integer, pattern = "(.*)_w([123])"))

dbl[, weekno := fifelse(date == "2021-11-27", 47, weekno)]
setkey(dbl, rid, BASELINE_Respondent_Serial, wave)



## merge mobility with dbl ----------------------------------------------------------

dbl <- mobility[dbl, on = .(sub_region_1 == CCAA, date)]

## merge stringency with dbl ----------------------------------------------------------

dbl <- stringency[dbl, on = .(Date == date)]
setnames(dbl, old = c("Date"), new = c("date"))


## other -------------------------------------------------------------------

dbl[, date := as.Date(date)]
dblclasses <- sapply(dbl |> zap_formats() |> zap_label() |> zap_labels() |> as.data.frame(), class)
cols <- names(dblclasses[which(dblclasses %in% c("integer", "numeric"))])
dbl[, c(cols) := lapply(.SD, nafill, fill = -93), .SDcols = cols]

cols <- names(dblclasses[which(!dblclasses %in% c("integer", "numeric"))])
dbl[, c(cols) := lapply(.SD, \(.x) fifelse(is.na(.x), "-93", as.character(.x))), .SDcols = cols]


setcolorder(dbl, neworder = c(match(names(stringency)[2], names(dbl)):match(tail(names(stringency),1), names(dbl)),
                              match(names(mobility)[3], names(dbl)):match(tail(names(mobility),1), names(dbl))),
            after = length(dbl))
setcolorder(dbl, c(key(dbl), "wave", "date", "weekno"))

dbl <- dbl[, !c("date", "sub_region_1")]

save(dbl, file = paste0(data_add, "../target/BBDD_long.rdata"))


## checks

# dbl[!is.na(date) & weekno == -93] |> print(topn=20, col.names = "top")
# udbl <- unique(dbl[order(BASELINE_Respondent_Serial),.(BASELINE_Respondent_Serial, rid)])
# lapply(cols,\(.x) table(dbl[[.x]], useNA = "always"))