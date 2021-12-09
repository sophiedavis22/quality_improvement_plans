library(tidyr)
library(dplyr)
library(readxl)
library(tidyverse)


# 1. First chunk of code for deriving the % of risks (QRs) affecting each dimension of quality ----


# Set a vector for expected null values
null_values <- c(NA, "-", "- ", "NA", "NA ", "N/A", "N/A ", "na", "na ", "n/a", "n/a ")


# Import list of files names in QIP folder
file <- list.files(path="//NDATA9/daviss1$/My Documents/Completed QIPs/", pattern="*.xlsx", full.names=TRUE, recursive=FALSE)
file_details_division <- as.data.frame(file)

# Trim file name to add division name
file_details_division$division_name <- str_replace_all(file_details_division$file, c("//NDATA9/daviss1\\$/My Documents/Completed QIPs/UPDATED_Quality_Improvement_Plan_"="", ".xlsx"=""))
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






key_metrics <- data.frame(matrix(nrow=4, ncol=2))
file_template <- "//NDATA9/daviss1$/My Documents/Completed QIPs/UPDATED_Quality_Improvement_Plan_division_name.xlsx"

#Import as one and remove NA rows?
key_metrics[1,] <- readxl::read_xlsx(file_template, sheet = "Key Metrics" , range = "A4:B4", col_names = FALSE, na = null_values)
key_metrics[2,] <- readxl::read_xlsx(file_template, sheet = "Key Metrics" , range = "A10:B10", col_names = FALSE, na = null_values)
key_metrics[3,] <- readxl::read_xlsx(file_template, sheet = "Key Metrics" , range = "A17:B17", col_names = FALSE, na = null_values)
key_metrics[4,] <- readxl::read_xlsx(file_template, sheet = "Key Metrics" , range = "A24:B24", col_names = FALSE, na = null_values)

colnames(key_metrics) <- c("id", "question")




# Need to extract division name
#outside loop
key_metrics_all_divisions <- list(NULL)

#inside loop
key_metrics_division_list <- list(NULL)

key_metrics_division_list[[1]] <- t(readxl::read_xlsx("//NDATA9/daviss1$/My Documents/Completed QIPs/UPDATED_Quality_Improvement_Plan_BPI.xlsx", sheet = "Key Metrics" , range = "B4:P8", col_names = FALSE, na = null_values))
key_metrics_division_list[[2]] <- t(readxl::read_xlsx("//NDATA9/daviss1$/My Documents/Completed QIPs/UPDATED_Quality_Improvement_Plan_BPI.xlsx", sheet = "Key Metrics" , range = "B10:P14", col_names = FALSE, na = null_values))
key_metrics_division_list[[3]] <- t(readxl::read_xlsx("//NDATA9/daviss1$/My Documents/Completed QIPs/UPDATED_Quality_Improvement_Plan_BPI.xlsx", sheet = "Key Metrics" , range = "B17:P21", col_names = FALSE, na = null_values))
key_metrics_division_list[[4]] <- t(readxl::read_xlsx("//NDATA9/daviss1$/My Documents/Completed QIPs/UPDATED_Quality_Improvement_Plan_BPI.xlsx", sheet = "Key Metrics" , range = "B24:P28", col_names = FALSE, na = null_values))

key_metrics_division_list[[1]][1,1]

# have an if statement for Q matching
for (i in 1:4) {
  print(i)
  if (key_metrics_division_list[[i]][1,1]!=key_metrics[i,2]) {
    stop("Question does not match")
  } else {
    print("Question matches")
  }
}





key_metrics_division_table <- data.frame(matrix(nrow=14, ncol=2))
key_metrics_division_table[,2] <- key_metrics_division_list[[1]][2:15,3]
colnames(key_metrics_division_table) <- c("division_name", "month")
rownames(key_metrics_division_table) <- NULL

for (i in 1:4) {
  metric <- key_metrics_division_list[[i]][2:15,4:5]
  colnames(metric) <- c(paste0("m",i), paste0("details_",i))
  rownames(metric) <- NULL
  key_metrics_division_table <- cbind(key_metrics_division_table, metric)
  print(key_metrics_division_table)
}


key_metric_responses <- lapply(key_metrics_division, '[[',3)
key_metric_responses


key_metrics_division[[1]][1,1]==key_metrics[1,2]
key_metrics_table_names

tkm1 <- t(key_metrics_m1)

header.true <- function(df) {
  names(df) <- as.character(unlist(df[1,]))
  df[-1,]
}

tkm1 <- header.true(tkm1)
colnames(tkm1) #<- 



key_metrics_m2 <- readxl::read_xlsx("//NDATA9/daviss1$/My Documents/Completed QIPs/UPDATED_Quality_Improvement_Plan_BPI.xlsx", sheet = "Key Metrics" , range = "B10:P14")
key_metrics_m3 <- readxl::read_xlsx("//NDATA9/daviss1$/My Documents/Completed QIPs/UPDATED_Quality_Improvement_Plan_BPI.xlsx", sheet = "Key Metrics" , range = "B17:P21")
key_metrics_m4 <- readxl::read_xlsx("//NDATA9/daviss1$/My Documents/Completed QIPs/UPDATED_Quality_Improvement_Plan_BPI.xlsx", sheet = "Key Metrics" , range = "B24:P28")

progress_check_q1 <- readxl::read_xlsx("//NDATA9/daviss1$/My Documents/Completed QIPs/UPDATED_Quality_Improvement_Plan_BPI.xlsx", sheet = "Key Metrics" , range = "B4:P10")
progress_check_q2 <- readxl::read_xlsx("//NDATA9/daviss1$/My Documents/Completed QIPs/UPDATED_Quality_Improvement_Plan_BPI.xlsx", sheet = "Key Metrics" , range = "B13:P19")
progress_check_q3 <- readxl::read_xlsx("//NDATA9/daviss1$/My Documents/Completed QIPs/UPDATED_Quality_Improvement_Plan_BPI.xlsx", sheet = "Key Metrics" , range = "B22:P28")
progress_check_q4 <- readxl::read_xlsx("//NDATA9/daviss1$/My Documents/Completed QIPs/UPDATED_Quality_Improvement_Plan_BPI.xlsx", sheet = "Key Metrics" , range = "B31:P37")
progress_check_q5 <- readxl::read_xlsx("//NDATA9/daviss1$/My Documents/Completed QIPs/UPDATED_Quality_Improvement_Plan_BPI.xlsx", sheet = "Key Metrics" , range = "B40:P46")


