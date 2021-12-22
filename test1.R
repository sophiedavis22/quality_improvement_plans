library(tidyr)
library(dplyr)
library(readxl)
library(tidyverse)


# 1. First chunk of code for deriving the % of risks (QRs) affecting each dimension of quality ----


# Set a vector for expected null values
null_values <- c(NA, "-", "- ", "NA", "NA ", "N/A", "N/A ", "na", "na ", "n/a", "n/a ")


# Import list of files names in QIP folder
file <- list.files(path="//NDATA9/daviss1$/My Documents/QIP work/Completed QIPs/", pattern="*.xlsx", full.names=TRUE, recursive=FALSE)


file_details_division <- as.data.frame(file)

# Trim file name to add division name
file_details_division$division_name <- str_replace_all(file_details_division$file, c("//NDATA9/daviss1\\$/My Documents/QIP work/Completed QIPs/UPDATED_Quality_Improvement_Plan_"="", ".xlsx"=""))
file_details_division
n_division <- nrow(file_details_division)


# Create empty list to store quality dimensions for each division in dfs
divisional_quality_dimensions <- list(NULL)
# Create empty matrix for total number of QRs for each division with division name
n_risks_division <- data.frame(matrix(nrow=n_division, ncol=2))
colnames(n_risks_division) <- c("division", "n_risks")


# Loop to extract dimension tables from each division (each file)
# i files
# j QRs (tabs)
for (i in 1:n_division) {
  print(i)

  # Select i-th file path and extract list of tabs, drawing out only those with QR data
  path <- file_details_division$file[i]
  tab_list <- excel_sheets(path = path)
  risks <- tab_list[!(tab_list %in% c("Contents", "Foreword by Sarah Henry", "Background", "Instructions", "Key Metrics", "BLANK Quality Risk", "Progress Check", "EXAMPLE Quality Risk"))]
  n_risk <- length(risks)

  # Recording the division name and number of risks in empty matrix
  n_risks_division[i,1] <- file_details_division$division_name[i]
  n_risks_division[i,2] <- n_risk


  # Create empty list within loop to temporarily store QRs for that division
  quality_risk_list <- list(NULL)

  # For j-th risk, read table from excel
  for (j in 1:n_risk) {
    quality_risk_list[[j]] <- readxl::read_xlsx(path, sheet = risks[j] , range = "B5:C10", na=null_values)
  }

  # Extract responses from each QR
  dimensions_responses <- lapply(quality_risk_list, '[[',2)

  # Bind all columns together into one df
  quality_dimensions_table <- as.data.frame(dimensions_responses)

  # Extract dimension names to set row names and create column names
  quality_dimensions_names <- unlist(quality_risk_list[[1]][1])
  rownames(quality_dimensions_table) <- quality_dimensions_names
  colnames(quality_dimensions_table) <- paste0("qr_", 1:n_risk)

  # Sum rows to give totals for each dimension, reorder so this column is first
  quality_dimensions_table$frequency <- rowSums(!is.na(quality_dimensions_table))
  quality_dimensions_table <- quality_dimensions_table[,c(n_risk+1, 1:n_risk)]

  # Save divisional table to empty list and rename list with division name
  divisional_quality_dimensions[[i]] <- quality_dimensions_table
  names(divisional_quality_dimensions)[i] <- file_details_division$division_name[i]
}

# View number of risks per division, and overall number of risks (sum)
n_risks_division
total_n_risks <- sum(n_risks_division$n_risks)

# Extract number of QRs affecting each dimensions of quality each division
all_divisions <- lapply(divisional_quality_dimensions, '[[',1)
all_divisions <- as.data.frame(all_divisions)
rownames(all_divisions) <- quality_dimensions_names

# Sum all rows to obtain total against each quality dimension and express as % of all QRs
all_divisions$total <- rowSums(all_divisions)
all_divisions$pct <- paste0(round(all_divisions$total/total_n_risks*100,2),"%")
all_divisions









# 2. Key metrics ----


key_metrics_ref <- data.frame(matrix(nrow=4, ncol=2))
file_template <- "//NDATA9/daviss1$/My Documents/QIP work/UPDATED_Quality_Improvement_Plan_division_name.xlsx"

months <- readxl::read_xlsx(file_template, sheet = "Key Metrics" , range = "c6:p6", col_names = FALSE, na = null_values)
years <- c(rep(2021,2), rep(2022,12))
reporting_period <- paste(months,years)
reporting_period <- factor(reporting_period, levels=reporting_period)


#Import as one and remove NA rows?
key_metrics_ref[1,] <- readxl::read_xlsx(file_template, sheet = "Key Metrics" , range = "A4:B4", col_names = FALSE, na = null_values)
key_metrics_ref[2,] <- readxl::read_xlsx(file_template, sheet = "Key Metrics" , range = "A10:B10", col_names = FALSE, na = null_values)
key_metrics_ref[3,] <- readxl::read_xlsx(file_template, sheet = "Key Metrics" , range = "A17:B17", col_names = FALSE, na = null_values)
key_metrics_ref[4,] <- readxl::read_xlsx(file_template, sheet = "Key Metrics" , range = "A24:B24", col_names = FALSE, na = null_values)

colnames(key_metrics_ref) <- c("q_id", "question")


key_metrics_all_divisions <- list(NULL)
# Create empty matrix for total number of months for each division with division name
n_months_division <- data.frame(matrix(nrow=n_division, ncol=2))
colnames(n_months_division) <- c("division", "n_months")





#key_metrics_division_table[,2] <- key_metrics_division_list[[1]][2:15,3]


for (i in 1:n_division) {
  print(i)
  key_metrics_division_list <- list(NULL)

  # Select i-th file path and extract list of tabs, drawing out only those with QR data
  path <- file_details_division$file[i]

  key_metrics_division_list[[1]] <- t(readxl::read_xlsx(path = path, sheet = "Key Metrics" , range = "B4:P8", col_names = FALSE, na = null_values))
  key_metrics_division_list[[2]] <- t(readxl::read_xlsx(path = path, sheet = "Key Metrics" , range = "B10:P14", col_names = FALSE, na = null_values))
  key_metrics_division_list[[3]] <- t(readxl::read_xlsx(path = path, sheet = "Key Metrics" , range = "B17:P21", col_names = FALSE, na = null_values))
  key_metrics_division_list[[4]] <- t(readxl::read_xlsx(path = path, sheet = "Key Metrics" , range = "B24:P28", col_names = FALSE, na = null_values))


  # have an if statement for Q matching
  for (j in 1:4) {
    print(j)
    if (key_metrics_division_list[[j]][1,1]!=key_metrics_ref[j,2]) {
      stop("Question does not match")
    } else {
      print("Question matches")
    }
  }
  years <- c(rep(2021,2), rep(2022,12))
  key_metrics_division_table <- data.frame(matrix(nrow=14, ncol=2))
  key_metrics_division_table[,2] <- paste(key_metrics_division_list[[1]][2:15,3], years)
  key_metrics_division_table[,1] <- file_details_division$division_name[i]
  colnames(key_metrics_division_table) <- c("division_name", "month")
  rownames(key_metrics_division_table) <- NULL

  for (k in 1:4) {
    metric <- key_metrics_division_list[[k]][2:15,4:5]
    colnames(metric) <- c(paste0("m",k), paste0("details_m",k))
    rownames(metric) <- NULL
    key_metrics_division_table <- cbind(key_metrics_division_table, metric)
  }

  for (l in 1:nrow(key_metrics_division_table)) {
    print(l)
    if (all(is.na(key_metrics_division_table[l,3:10]))) {
      key_metrics_division_table[l,1] <- NA
      key_metrics_division_table[l,2] <- NA
    } else {
      key_metrics_division_table[l,1] <- key_metrics_division_table[l,1]
      key_metrics_division_table[l,2] <- key_metrics_division_table[l,2]
    }
  }

  key_metrics_division_table_final <- key_metrics_division_table[rowSums(is.na(key_metrics_division_table))!=ncol(key_metrics_division_table),]
  names <- c("m1" ,"m2", "m3", "m4")
  key_metrics_division_table_final[,names] <- lapply(key_metrics_division_table_final[,names] , as.numeric)

  n_months <- nrow(key_metrics_division_table_final)

  # Recording the division name and number of risks in empty matrix
  n_months_division[i,1] <- file_details_division$division_name[i]
  n_months_division[i,2] <- n_months

  key_metrics_all_divisions[[i]] <- key_metrics_division_table_final
}

View(key_metrics_division_list[[1]])
View(key_metrics_all_divisions[[1]])




key_metrics_all_divisions_table_with_details <- do.call(rbind, key_metrics_all_divisions)


key_metrics_all_divisions_table <- subset(key_metrics_all_divisions_table_with_details, select=-c(details_m1, details_m2, details_m3, details_m4))


output_key_metrics <- aggregate(. ~month, key_metrics_all_divisions_table[,-1], sum)
output_key_metrics <- output_key_metrics[match(reporting_period, output_key_metrics$month),]

output_key_metrics_final <- output_key_metrics[rowSums(is.na(output_key_metrics))!=ncol(output_key_metrics),]
rownames(output_key_metrics_final) <- NULL


key_metrics_present <- data.frame(question=paste(key_metrics_ref$q_id, key_metrics_ref$question),
                                  previous_month=t(output_key_metrics_final[nrow(output_key_metrics_final)-1,2:5]),
                                  latest_month=t(output_key_metrics_final[nrow(output_key_metrics_final),2:5]))
colnames(key_metrics_present) <- c("Question",
                                   paste0("Previous month (",output_key_metrics_final[nrow(output_key_metrics_final)-1,1], ")"),
                                   paste0("Latest month (", output_key_metrics_final[nrow(output_key_metrics_final),1], ")"))
rownames(key_metrics_present) <- NULL


key_metrics_present













progress_check_q1 <- readxl::read_xlsx("//NDATA9/daviss1$/My Documents/Completed QIPs/UPDATED_Quality_Improvement_Plan_BPI.xlsx", sheet = "Key Metrics" , range = "B4:P10")
progress_check_q2 <- readxl::read_xlsx("//NDATA9/daviss1$/My Documents/Completed QIPs/UPDATED_Quality_Improvement_Plan_BPI.xlsx", sheet = "Key Metrics" , range = "B13:P19")
progress_check_q3 <- readxl::read_xlsx("//NDATA9/daviss1$/My Documents/Completed QIPs/UPDATED_Quality_Improvement_Plan_BPI.xlsx", sheet = "Key Metrics" , range = "B22:P28")
progress_check_q4 <- readxl::read_xlsx("//NDATA9/daviss1$/My Documents/Completed QIPs/UPDATED_Quality_Improvement_Plan_BPI.xlsx", sheet = "Key Metrics" , range = "B31:P37")
progress_check_q5 <- readxl::read_xlsx("//NDATA9/daviss1$/My Documents/Completed QIPs/UPDATED_Quality_Improvement_Plan_BPI.xlsx", sheet = "Key Metrics" , range = "B40:P46")


