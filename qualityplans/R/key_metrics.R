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
    #trimmed_metric$details[!is.na(trimmed_metric$count)&is.na(trimmed_metric$details)] <- paste0("N/A", as.character(names(x)))

    return(trimmed_metric)
  })

}



merge_metric_table <- function(metric_list) {
  metric_reduced <- trim_metric_table(metric_list)
  merged_metrics_table <- data.frame(metric_reduced)
  colnames(merged_metrics_table) <- rep(c("month", "count", "details"),4)
  id.rm <- which(duplicated(names(merged_metrics_table)) & names(merged_metrics_table) == "month")
  merged_metrics_table <- merged_metrics_table[,-id.rm]
  colnames(merged_metrics_table) <- c("month", paste0(c("m", "details_m"), rep(1:4,each=2)))
  rownames(merged_metrics_table) <- NULL

  years <- c(rep(2021,2), rep(2022,12))
  merged_metrics_table$month <- paste(merged_metrics_table$month, years)
  merged_metrics_table$month <- factor(merged_metrics_table$month, levels=merged_metrics_table$month)


  return(merged_metrics_table)
}






get_all_merged_metric_tables <- function(all_metric_list) {
  lapply(all_metric_list, function(x){
    all_merged_metrics_tables <- merge_metric_table(x)
    names <- c("m1" ,"m2", "m3", "m4")
    all_merged_metrics_tables[,names] <- lapply(all_merged_metrics_tables[,names] , as.numeric)
    return(all_merged_metrics_tables)
  })
}




combine_metrics_single_table <- function(all_divisions_metrics_tables) {
  stacked_metrics <- do.call(rbind, all_divisions_metrics_tables)
  stacked_metrics$listname <- lapply(strsplit(row.names(stacked_metrics), "\\."), '[[', 1)
  stacked_metrics <- stacked_metrics[,c(ncol(stacked_metrics),1:(ncol(stacked_metrics)-1))]
  stacked_metrics <- stacked_metrics[rowSums(is.na(stacked_metrics[,-c(1,2)]))!=(ncol(stacked_metrics)-2),]
  colnames(stacked_metrics)[1] <- "division"
  rownames(stacked_metrics) <- NULL
  return(stacked_metrics)

}

combine_sum_metric_table <- function(all_divisions_metrics_tables) {
  stacked_metrics <- combine_metrics_single_table(all_divisions_metrics_tables)
  table_no_details <- subset(stacked_metrics, select=-c(details_m1, details_m2, details_m3, details_m4))
  summed_metric_table <- aggregate(. ~month, table_no_details[,-1], sum)
  return(summed_metric_table)
}

format_metric_table <- function(all_divisions_metrics_tables) {
  config_file <- read_config()

  summed_metric_table <- combine_sum_metric_table(all_divisions_metrics_tables)
  metric_names <- c("", "m1" ,"m2", "m3", "m4")
  previous_month <- config_file$reporting_months$previous_month
  this_month <- config_file$reporting_months$this_month

  this_month_v <- summed_metric_table[summed_metric_table$month==this_month,]
  previous_month_v <- summed_metric_table[summed_metric_table$month==previous_month,]

  output_table <- data.frame(q_id=metric_names, this_month=t(this_month_v), previous_month=t(previous_month_v))

  colnames(output_table) <- c("Question",
                     #paste0("Previous month (",previous_month, ")"),
                     paste0("Latest month (", this_month, ")"))
  output_table <- output_table[-1,]
  rownames(output_table) <-NULL

  return(output_table)

}







