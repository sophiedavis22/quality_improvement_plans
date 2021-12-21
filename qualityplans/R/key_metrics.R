#' @title
#'
#' @description
#'
#' @param
#' @param
#'
#' @return


remove_na_rows <- function(table) {
  table_out <- table[!rowSums(is.na(table[,-c(1:2)])) == 8, ]       # Apply rowSums & is.na
  return(table_out)
}







#' @title
#'
#' @description
#'
#' @param
#' @param
#'
#' @return

extract_metric_tables <- function(file_path) {
  config_file <- read_config()
  lapply(config_file$metric_range , function(x){
    metric_list <- t(readxl::read_xlsx(file_path, sheet = "Key Metrics", range = x, col_names=FALSE, na=config_file$null_values))
    return(metric_list)
  })
}

#metric_list$division <- get_division_name(x)
#file_path_list[[1]]
#out <- extract_metric_tables(file_path_list[[1]])
#out_1 <- out[[1]]
#get_division_name(file_path_list[[1]])

#' @title
#'
#' @description
#'
#' @param
#' @param
#'
#' @return

extract_metric_tables_all <- function(file_path){
  lapply(file_path, function(x){
    all_metric_list <- extract_metric_tables(x)
    return(all_metric_list)
  })
}



#' @title
#'
#' @description
#'
#' @param
#' @param
#'
#' @return

get_metric_tables_all <- function(file_path, division_names) {
  all_metric_list <- extract_metric_tables_all(file_path)
  all_metric_list_named <- assign_division_names(all_metric_list, division_names)
  return(all_metric_list_named)
}







#' @title
#'
#' @description
#'
#' @param
#' @param
#'
#' @return


trim_metric_table <- function(metric_list) {
  lapply(metric_list, function(x){
    trimmed_metric <- as.data.frame(x[2:15,3:5])
    colnames(trimmed_metric) <- c("month", "count", "details")
    rownames(trimmed_metric) <- NULL
    trimmed_metric$details[!is.na(trimmed_metric$count)&is.na(trimmed_metric$details)] <- paste0("N/A", as.character(names(x)))

    return(trimmed_metric)
  })

}


#paste0("N/A_", as.character(names(all_metric_list)))


#red_1 <- trim_metric_table(all_metric_list[[3]])



#red_2 <- get_merged_metric_table(all_metric_list[[3]])
#red_3 <- get_merged_metric_table(all_metric_list[[3]])
#red_2



#a$details[!is.na(a$count)] <- "N/A"


get_merged_metric_table <- function(metric_list){
  metric_reduced <- trim_metric_table(metric_list)
  merged_metrics_table <- data.frame(metric_reduced)
  merged_metrics_table <- merged_metrics_table[!duplicated(as.list(merged_metrics_table))]
  #colnames(merged_metrics_table) <- c("month", paste0(c("m", "details_m"), rep(1:4,each=2)))
  rownames(merged_metrics_table) <- NULL
  return(merged_metrics_table)
}






get_all_merged_metric_tables <- function(all_metric_list) {
  lapply(all_metric_list, function(x){
    all_merged_metrics_tables <- get_merged_metric_table(x)
    return(all_merged_metrics_tables)
  })
}

#out_tabs <- get_all_merged_metric_tables(all_metric_list)
#out_1 <- out_tabs[[1]]
#out_2 <- out_tabs[[2]]

#all_2 <- all_metric_list[[2]][[3]]
