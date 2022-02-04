#' @title Extract all metric tables from file
#'
#' @description Function to extract metric tables from divisional excel file
#'
#' @param file_path File path for single division
#' @param config_file .yaml file containing config, with default settings (data directory, null values, metric ranges etc)
#'
#' @return List of 4 tables, one for each metric

extract_metrics_from_tabs <- function(file_path, config_file="D:/Repos/quality_improvement_plans/config.yaml") {
  config <- read_config(config_file)
  tab_list <- readxl::excel_sheets(path = file_path)
  risk_tabs <- tab_list %in% "Key Metrics"

  if (!any(risk_tabs)==FALSE) {
    lapply(config$metric_range , function(x){
      metric_list <- t(readxl::read_xlsx(file_path, sheet = "Key Metrics", range = x, col_names=FALSE, na=config$null_values))
      rownames(metric_list) <- NULL
      metric_list <- as.data.frame(metric_list)
      return(metric_list)
    })
  } else {
    stop(paste(file_path, "has missing Key metrics tab"))
  }
}



#' @title Extract all metric tables from files
#'
#' @description Function to extract all metric tables from all excel files, returning a list of metrics tables in a list by division
#'
#' @param file_paths Character vector of file paths
#' @param config_file .yaml file containing config, with default settings (data directory, null values, metric ranges etc)
#'
#' @return List with sub-lists

extract_all_metrics_unnamed <- function(file_paths, config_file="D:/Repos/quality_improvement_plans/config.yaml"){
  config <- read_config(config_file)
  lapply(file_paths, function(x){
    all_metric_list <- extract_metrics_from_tabs(x)
    return(all_metric_list)
  })
}



#' @title Extract all metric tables from files (with named list!)
#'
#' @description Function to extract all metric tables from all excel files, returning a list of metrics tables in a named list by division
#'
#' @param file_paths Character vector of file paths
#' @param new_list_names New names as values
#' @param config_file .yaml file containing config, with default settings (data directory, null values, metric ranges etc)
#'
#' @return Named list with sub-lists

get_all_metrics_list <- function(file_paths, new_list_names, config_file="D:/Repos/quality_improvement_plans/config.yaml") {
  config <- read_config(config_file)
  all_metric_list <- extract_all_metrics_unnamed(file_paths)
  all_metric_list_named <- assign_list_names(all_metric_list, new_list_names)
  return(all_metric_list_named)
}


#=============================================================================


#' @title Trim all metric tables
#'
#' @description Function to remove first two columns and convert count to numeric
#'
#' @param metric_list List containing 4 metric tables for one division
#' @param config_file .yaml file containing config, with default settings (data directory, null values, metric ranges etc)
#'
#' @return List of trimmed tables


trim_metric_table <- function(metric_list, config_file="D:/Repos/quality_improvement_plans/config.yaml") {
  config <- read_config(config_file)
  lapply(metric_list, function(x){
    trimmed_metric <- as.data.frame(x[2:(config$metric_months$n_months+1),3:5])
    colnames(trimmed_metric) <- c("month", "count", "details")
    rownames(trimmed_metric) <- NULL

    test <- tryCatch(as.numeric(trimmed_metric$count), error=function(e) e, warning=function(w) w)

    if (is(test,"warning")==FALSE) {
      trimmed_metric$count <- as.numeric(trimmed_metric$count)
      return(trimmed_metric)
    } else {
      stop(paste(x[1,1],"count must be numeric"))
    }
  })
}



#' @title Merge metrics for one division into one table
#'
#' @description Function to merge all 4 metrics into one table for one division
#'
#' @param metric_list List containing 4 metric tables for one division
#' @param config_file .yaml file containing config, with default settings (data directory, null values, metric ranges etc)
#'
#' @return Table with all metrics for one division

merge_metric_table <- function(metric_list, config_file="D:/Repos/quality_improvement_plans/config.yaml") {
  config <- read_config(config_file)
  test <- tryCatch(trim_metric_table(metric_list), error=function(e) e, warning=function(w) w)

  if(is(test, "error")==FALSE) {
    metric_reduced <- trim_metric_table(metric_list, config_file)
    merged_metrics_table <- data.frame(metric_reduced)
    colnames(merged_metrics_table) <- rep(c("month", "count", "details"),4)
    id.rm <- which(duplicated(names(merged_metrics_table)) & names(merged_metrics_table) == "month")
    merged_metrics_table <- merged_metrics_table[,-id.rm]
    colnames(merged_metrics_table) <- c("month", paste0(c("m", "details_m"), rep(1:4,each=2)))
    rownames(merged_metrics_table) <- NULL

    years <- c(config$metric_months$years)
    month_year <- paste(merged_metrics_table$month, years)
    merged_metrics_table$month <- month_year

    merged_metrics_table$month <- factor(month_year, levels=month_year)
    return(merged_metrics_table)

  } else {
    metric_reduced <- trim_metric_table(metric_list)
    return(metric_reduced)
  }
}



#' @title Merge metrics for each division into one table
#'
#' @description Function to merge all 4 metrics into one table for all divisions in list
#'
#' @param all_metric_list List by division containing 4 metric tables for each division
#' @param config_file .yaml file containing config, with default settings (data directory, null values, metric ranges etc)
#'
#' @return List containing one table for each division


get_all_merged_metric_tables <- function(all_metric_list, config_file="D:/Repos/quality_improvement_plans/config.yaml") {
  config <- read_config(config_file)
  lapply(all_metric_list, function(x){
    all_merged_metrics_tables <- merge_metric_table(x, config_file)
    #names <- c("m1" ,"m2", "m3", "m4")
    #all_merged_metrics_tables[,names] <- lapply(all_merged_metrics_tables[,names] , as.numeric)
    return(all_merged_metrics_tables)
  })
}


# ###############################################################




#' @title Combine all data from metrics into single table
#'
#' @description Function to pull together detailed data from all divisions into single tidy data frame
#'
#' @param division_metric_list List by division with sub-lists for each metric
#'
#' @return Tidy data frame with all data for all divisions


combine_metrics <- function(division_metric_list) {
  stacked_metrics <- do.call(rbind, division_metric_list)
  stacked_metrics$listname <- lapply(strsplit(row.names(stacked_metrics), "\\."), '[[', 1)
  stacked_metrics <- stacked_metrics[,c(ncol(stacked_metrics),1:(ncol(stacked_metrics)-1))]
  stacked_metrics <- stacked_metrics[rowSums(is.na(stacked_metrics[,-c(1,2)]))!=(ncol(stacked_metrics)-2),]
  colnames(stacked_metrics)[1] <- "division"
  rownames(stacked_metrics) <- NULL
  stacked_metrics$division <- as.character(stacked_metrics$division)
  return(stacked_metrics)

}


#' @title Combine all numeric data from metrics into single table
#'
#' @description Function to pull together reported figures from all divisions into single table
#'
#' @param division_metric_list List by division with sub-lists for each metric
#'
#' @return Table with returns for each metric for each month

aggregate_metrics <- function(division_metric_list) {
  stacked_data <- combine_metrics(division_metric_list)
  table_no_details <- subset(stacked_data, select=-c(details_m1, details_m2, details_m3, details_m4))
  if (any(is.na(table_no_details[,-1]))) {
    na_rows <- table_no_details[!complete.cases(table_no_details), 1]
    stop("Divisions (", paste0(na_rows, ", "), ") have unexpected NA values")


  } else {
    sum_metric_table <- aggregate(. ~month, table_no_details[,-1], sum)
    return(sum_metric_table)
  }

}


### UNIT TESTS completed to this line
########################################################################################################


#' @title Get table with latest 2 months of data
#'
#' @description Function to condense metric data from all divisions to figures only for latest 2 months
#'
#' @param all_metric_list List by division containing 4 metric tables for each division
#' @param config_file .yaml file containing config, with default settings (data directory, null values, metric ranges etc)
#'
#' @return Single table with latest 2 months of data

recent_metric_table <- function(all_metric_list, config_file="D:/Repos/quality_improvement_plans/config.yaml") {
  config <- read_config(config_file)

  summed_metric_table <- aggregate_metrics(all_metric_list)
  metric_names <- c("", "M1" ,"M2", "M3", "M4")
  previous_month <- config$reporting_months$previous_month
  latest_month <- config$reporting_months$latest_month

  latest_month_v <- summed_metric_table[summed_metric_table$month==latest_month,]
  previous_month_v <- summed_metric_table[summed_metric_table$month==previous_month,]

  output_table <- data.frame(q_id=metric_names, previous_month=t(previous_month_v), latest_month=t(latest_month_v))

  colnames(output_table) <- c("Question",
                              paste0("Previous month (",previous_month, ")"),
                              paste0("Latest month (", latest_month, ")"))
  output_table <- output_table[-1,]
  rownames(output_table) <-NULL

  output_table[,-1] <- as.numeric(unlist(output_table[,-1]))
  return(output_table)
}






