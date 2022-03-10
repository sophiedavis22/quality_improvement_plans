devtools::install("qualityplans")

#source("R/utilities.R")
#source("R/quality_risks.R")
#source("R/key_metrics.R")

qip_config <- qualityplans::read_config()

qip_file_paths <- qualityplans::get_file_paths()
qip_file_paths
qip_division_names <- qualityplans::get_division_names(qip_file_paths)
qip_division_names


qip_divisions_risks_list <- qualityplans::get_all_risks_list(qip_file_paths, qip_division_names)
qip_divisions_risks_merged <- qualityplans::get_risk_dimension_tables(qip_divisions_risks_list, qip_division_names)
final_table_qip_dimension_risk <- qualityplans::get_final_risk_dimension_table(qip_divisions_risks_merged)







qip_metrics_list <- qualityplans::get_all_metrics_list(qip_file_paths, qip_division_names)
qip_metric_tables <- qualityplans::get_all_merged_metric_tables(qip_metrics_list)

qip_metrics_data_frame <- qualityplans::combine_metrics(qip_metric_tables)


final_table_qip_metrics <- qualityplans::recent_metric_table(qip_metric_tables)

devtools::load("qualityplans")
getwd()
setwd("..")
devtools::load_all("qualityplans")
devtools::document("qualityplans")
devtools::install("qualityplans")
