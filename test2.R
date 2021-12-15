library(readxl)
library(tidyverse)

source("R/utilities.R")
source("R/quality_risks.R")
qip_config <- read_config()

file_path_list <- get_file_path_list()
file_path_list
all_division_names <- get_division_name_list(file_path_list)
all_division_names

all_divisions_risks_sep <- get_sep_risks_all_div(file_path_list, all_division_names)
all_divisions_risks_merged <- get_merged_risks_all_div(all_divisions_risks_sep, all_division_names)

final_table <- get_final_risk_dimension_table(all_divisions_risks_merged)
