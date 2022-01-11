devtools::install("qualityplans")
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
all_divisions_metrics_tables <- get_all_merged_metric_tables(all_metric_list)
all_divisions_metrics <- combine_metrics_single_table(all_divisions_metrics_tables)
metrics_table_final <- format_metric_table(all_divisions_metrics_tables)

devtools::load("qualityplans")
getwd()
setwd("..")
devtools::load_all("qualityplans")
devtools::document("qualityplans")
devtools::install("qualityplans")
