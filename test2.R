devtools::install("qualityplans")
#library(tidyverse)

source("R/utilities.R")
source("R/quality_risks.R")
source("R/key_metrics.R")

qip_config <- read_config()

qip_file_paths <- get_file_paths()
qip_file_paths
qip_division_names <- get_division_names(qip_file_paths)
qip_division_names


qip_divisions_risks_list <- get_all_risks_list(qip_file_paths, qip_division_names)
qip_divisions_risks_merged <- get_risk_dimension_tables(qip_divisions_risks_list, qip_division_names)
dimension_risk_final_table <- get_final_risk_dimension_table(qip_divisions_risks_merged)







all_metric_list <- get_all_metrics_list(qip_file_paths, qip_division_names)
all_divisions_metrics_tables <- get_all_merged_metric_tables(all_metric_list)
all_divisions_metrics <- combine_sum_metrics(all_divisions_metrics_tables)



metrics_table_final <- format_metric_table(all_divisions_metrics_tables)

devtools::load("qualityplans")
getwd()
setwd("..")
devtools::load_all("qualityplans")
devtools::document("qualityplans")
devtools::install("qualityplans")
