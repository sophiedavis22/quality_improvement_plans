library(readxl)
library(tidyverse)

source("R/utilities.R")
source("R/quality_risks.R")
qip_config <- read_config()

null_values <- c(NA, "-", "- ", "NA", "NA ", "N/A", "N/A ", "na", "na ", "n/a", "n/a ")
file_path_list <- list.files(path=qip_config$directory, pattern="*.xlsx", full.names=TRUE, recursive=FALSE)
risks <- extract_all_division_risks(file_path_list)
out <- create_all_division_risk_tables(risks)
all_risks <- merge_all_division_risks(out)
