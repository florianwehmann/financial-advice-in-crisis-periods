library(data.table)
library(haven)
library(lubridate)
library(readxl)

rm(list = ls())
gc()

data_dir <- "C:/Users/FWehmann/Dropbox/Financial Advice in Crisis Periods/Empirical Analysis/Build/Data/"
emp_dir  <- "C:/Users/FWehmann/Dropbox/Financial Advice in Crisis Periods/Empirical Analysis/"

# ------------------------------------------------------------------------------
# One-time: convert all .dta files in data_dir to .rds
# dta_list <- list.files(data_dir)[grepl(".dta", list.files(data_dir))]
# for (f in dta_list) {
#   temp <- as.data.table(read_dta(paste0(data_dir, f)))
#   saveRDS(temp, paste0(data_dir, gsub(".dta", ".rds", f)))
# }

# One-time: convert master.dta -> master.rds
# temp <- as.data.table(read_dta(paste0(emp_dir, "Build/Analysis/master.dta")))
# saveRDS(temp, paste0(emp_dir, "Build/Analysis/master.rds"))

# ------------------------------------------------------------------------------
# Load all .rds files from data_dir into global environment
dta_list  <- list.files(data_dir)[grepl(".dta", list.files(data_dir))]
file_list <- gsub(".dta", "", dta_list)

for (f in file_list) {
  assign(f, readRDS(file.path(data_dir, paste0(f, ".rds"))))
}
