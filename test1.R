install.packages("readxl")
install.packages("rlist")
library(tidyr)
library(dplyr)
library(readxl)



null_values <- c(NA, "-", "- ", "NA", "NA ", "N/A", "N/A ", "na", "na ", "n/a", "n/a ")


path <- "//NDATA9/daviss1$/My Documents/Completed QIPs/UPDATED_Quality_Improvement_Plan_BPI.xlsx"
tab_list <- excel_sheets(path = path)

tab_list[!(tab_list %in% c("Contents", "Foreword by Sarah Henry", "Background", "Instructions", "Key Metrics", "BLANK Quality Risk", "Progress Check", "EXAMPLE Quality Risk"))]


n_risk <- 2

quality_risk_list <- list(NULL)

for (i in 1:n_risk) {
  quality_risk_list[[i]] <- readxl::read_xlsx("//NDATA9/daviss1$/My Documents/Completed QIPs/UPDATED_Quality_Improvement_Plan_BPI.xlsx", sheet = paste0("Quality Risk ",i) , range = "B5:C10", na=null_values)
  assign(paste0("quality_dimension_",i), quality_risk_list[[i]])
}


quality_dimensions_table <- rbindlist(quality_risk_list, use.names=TRUE, fill=FALSE)

quality_dimensions_table$frequency <- rowSums(!is.na(quality_dimensions_table[-1]))






?rbind

key_metrics_m1 <- readxl::read_xlsx("//NDATA9/daviss1$/My Documents/Completed QIPs/UPDATED_Quality_Improvement_Plan_BPI.xlsx", sheet = "Key Metrics" , range = "B4:P8")
key_metrics_m2 <- readxl::read_xlsx("//NDATA9/daviss1$/My Documents/Completed QIPs/UPDATED_Quality_Improvement_Plan_BPI.xlsx", sheet = "Key Metrics" , range = "B10:P14")
key_metrics_m3 <- readxl::read_xlsx("//NDATA9/daviss1$/My Documents/Completed QIPs/UPDATED_Quality_Improvement_Plan_BPI.xlsx", sheet = "Key Metrics" , range = "B17:P21")
key_metrics_m4 <- readxl::read_xlsx("//NDATA9/daviss1$/My Documents/Completed QIPs/UPDATED_Quality_Improvement_Plan_BPI.xlsx", sheet = "Key Metrics" , range = "B24:P28")

progress_check_q1 <- readxl::read_xlsx("//NDATA9/daviss1$/My Documents/Completed QIPs/UPDATED_Quality_Improvement_Plan_BPI.xlsx", sheet = "Key Metrics" , range = "B4:P10")
progress_check_q2 <- readxl::read_xlsx("//NDATA9/daviss1$/My Documents/Completed QIPs/UPDATED_Quality_Improvement_Plan_BPI.xlsx", sheet = "Key Metrics" , range = "B13:P19")
progress_check_q3 <- readxl::read_xlsx("//NDATA9/daviss1$/My Documents/Completed QIPs/UPDATED_Quality_Improvement_Plan_BPI.xlsx", sheet = "Key Metrics" , range = "B22:P28")
progress_check_q4 <- readxl::read_xlsx("//NDATA9/daviss1$/My Documents/Completed QIPs/UPDATED_Quality_Improvement_Plan_BPI.xlsx", sheet = "Key Metrics" , range = "B31:P37")
progress_check_q5 <- readxl::read_xlsx("//NDATA9/daviss1$/My Documents/Completed QIPs/UPDATED_Quality_Improvement_Plan_BPI.xlsx", sheet = "Key Metrics" , range = "B40:P46")



xlsx_test$frequency <- rowSums(!is.na(xlsx_test[-1]))

