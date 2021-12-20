library(readxl)
library(tidyverse)

source("R/utilities.R")
source("R/quality_risks.R")
source("R/key_metrics.R")

qip_config <- read_config()

file_path_list <- get_file_path_list()
file_path_list
all_division_names <- get_division_name_list(file_path_list)
all_division_names

all_divisions_risks_sep <- get_sep_risks_all_div(file_path_list, all_division_names)
all_divisions_risks_merged <- get_merged_risks_all_div(all_divisions_risks_sep, all_division_names)

dimension_risk_final_table <- get_final_risk_dimension_table(all_divisions_risks_merged)





all_metric_list <- get_metric_tables_all(file_path_list, all_division_names)

all_metric_list[[2]]

out.table <- get_all_merged_metric_tables(all_metric_list)


file_path_1 <- file_path_list[1]

metric_list_1 <- extract_metric_tables(file_path_1)
metric_list_1[[1]]

table <- data.frame(x = 1:4, y = c(5:7, NA), z = c(10:11, NA, NA))
