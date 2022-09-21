# BcnMadCEdata_arr.r

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


