#' @title Extract Quality Risks for single division
#'
#' @description Function to extract Quality Risk tables from multiple excel tabs in one divisional file
#'
#' @param file_path File path for single division
#' @param risk_tabs Tab names for division
#' @param config_file .yaml file containing config, with default settings (data directory, null values, metric ranges etc)
#'
#' @return List of tables (output table for each risk)

extract_risks_from_tabs <- function(file_path, risk_tabs, config_file="D:/Repos/quality_improvement_plans/config.yaml"){
  config <- read_config(config_file)
  lapply(risk_tabs, function(x){
    quality_risk_list <- readxl::read_xlsx(file_path, sheet = x , range = config$dimension_range, na=config$null_values)
    return(quality_risk_list)
    })
}





#' @title Extract Quality Risks for all divisions
#'
#' @description Function to extract tab names (for each division) and extract Quality Risk tables for all divisions
#'
#' @param file_paths Character vector of file paths
#' @param config_file .yaml file containing config, with default settings (data directory, null values, metric ranges etc)
#'
#' @return List of tables (list by division, sub-lists by risk)

extract_all_risks_unnamed <- function(file_paths, config_file="D:/Repos/quality_improvement_plans/config.yaml"){
  config <- read_config(config_file)
  lapply(file_paths, function(x){
    tab_list <- readxl::excel_sheets(path = x)
    risk_tabs <- tab_list[!(tab_list %in% config$non_risk_tab_names)]
    risk_list <- extract_risks_from_tabs(x, risk_tabs)

    risk_names <- paste0("qr_", 1:length(risk_tabs))
    risk_list <- assign_list_names(risk_list, risk_names)
    return(risk_list)
  })
}



#' @title Extract Quality Risks for all divisions (with named list!)
#'
#' @description Function to extract tab names (for each division), extract Quality Risk tables for all divisions and name list elements
#'
#' @param file_paths Character vector of file paths
#' @param new_list_names  New names as values
#' @param config_file .yaml file containing config, with default settings (data directory, null values, metric ranges etc)
#'
#' @return List of tables (list by division, sub-lists of tibbles for each risk)

get_all_risks_list <- function(file_paths, new_list_names, config_file="D:/Repos/quality_improvement_plans/config.yaml") {
  all_risk_list <- extract_all_risks_unnamed(file_paths, config_file)
  all_risk_list <- assign_list_names(all_risk_list, new_list_names)
  return(all_risk_list)
}



#' @title Sum over non-empty rows
#'
#' @description Function to sum non-empty cells in rows (excluding first row as row name)
#'
#' @param table Table with characters
#'
#' @return Table with added frequency column

sum_full_cells <- function(table){
  table$frequency <- rowSums(!is.na(table[-1]))
  return(table)
}



#' @title Collate Quality Risks for single division
#'
#' @description Function to merge all Quality Risks for one division into one table, with row totals for number of risks affecting each dimension
#'
#' @param risk_list List of quality risks for single division
#' @param division_order Numeric vector from 1 to number of divisions
#'
#' @return Table with total column

merge_risks <- function(risk_list, division_order){
  risk_table <- data.frame(risk_list[[division_order]])
  risk_table <- risk_table[!duplicated(as.list(risk_table))]
  risk_table <- sum_full_cells(risk_table)

  colnames(risk_table) <- c("dimension", paste0("qr_",1:(ncol(risk_table)-2)), "total")
  return(risk_table)
  }



#' @title Collate Quality Risks in tables for each division
#'
#' @description Function to produce table of risks for each division, with row totals for number of risks affecting each dimension
#'
#' @param all_risk_list List of quality risks (by division) containing sub lists of risks
#'
#' @return List of tables (not named)

merge_all_risks <- function(all_risk_list){
  lapply(1:length(all_risk_list), function(x){
    each_div_risk_table <- merge_risks(all_risk_list, x)
    return(each_div_risk_table)
    })
  }



#' @title Collate Quality Risks in tables for each division (adding list element names)
#'
#' @description Function to produce table of risks for each division, with row totals for number of risks affecting each dimension and named list elements
#'
#' @param all_risk_list List of quality risks (by division) containing sub lists of risks
#' @param new_list_names New names as values
#'
#' @return List of tables (named)

get_risk_dimension_tables <- function(all_risk_list, new_list_names) {
  merged_risks <- merge_all_risks(all_risk_list)
  merged_risks <- assign_list_names(merged_risks, new_list_names)
  return(merged_risks)
}



#' @title Sum over numerical rows
#'
#' @description Function to sum numerical cells in rows (excluding first row as row name)
#'
#' @param table Table with character row names and numeric entries
#'
#' @return Table with added "total" column

sum_num <- function(table){
  table$total <- rowSums(table, na.rm=TRUE)
  return(table)
}



#' @title Create single division/dimension table - numbers only
#'
#' @description Function to create a single table with dimension breakdown by division, and sum for each dimension
#'
#' @param merged_risks List of risk tables by division
#'
#' @return Table of risks by division and division with added "total" column

create_dim_table <- function(merged_risks){
  dim_table <- data.frame(merged_risks)
  dimension_names <- dim_table[,1]
  dim_table <- dim_table[,-grep("dimension", colnames(dim_table))]
  dim_table <- dim_table[,-grep("qr", colnames(dim_table))]
  dim_table <- sum_num(dim_table)

  colnames(dim_table) <- gsub(".total", "", colnames(dim_table))
  dim_table <- cbind(dimension_names, dim_table)
  colnames(dim_table)[1] <- "dimension"
  return(dim_table)
}



#' @title Count number of quality risks per division
#'
#' @description Function to count number of quality risks in each division from list of risks by division
#'
#' @param merged_risks List of risk tables by division
#'
#' @return List with number of risks for each division

count_risks <- function(merged_risks){
  lapply(merged_risks, function(x){
    # don't count row names or totals
    risk_count_list <- as.numeric(ncol(x)-2)
    return(risk_count_list)
  })
}



#' @title Number of Quality Risks by division in table
#'
#' @description Function to extract number of quality risk from list of totals by division
#'
#' @param merged_risks List of risk tables by division
#'
#' @return Table of totals by division

create_risk_table <- function(dim_table){
  risk_count_list <- count_risks(dim_table)
  n_risk_table <- data.frame(risk_count_list)
  n_risk_table <- sum_num(n_risk_table)
  n_risk_table$dimension <- "No. quality risks"
  n_risk_table <- n_risk_table[c(ncol(n_risk_table),1:(ncol(n_risk_table)-1))]
  return(n_risk_table)
}



#' @title Table of quality risks by division and dimension
#'
#' @description Function to produce table with all risks by dimension and division, with final % of Quality risks affecting each division
#'
#' @param merged_risks List of risk tables by division
#'
#' @return Table

get_final_risk_dimension_table <- function(merged_risks){
  table_by_dimension <- create_dim_table(merged_risks)
  table_by_risk <- create_risk_table(merged_risks)
  total_n_risks <- table_by_risk$total

  final_table <- rbind(table_by_dimension, table_by_risk)

  final_table$pct <- as_percent(final_table$total/total_n_risks)
  return(final_table)
}

