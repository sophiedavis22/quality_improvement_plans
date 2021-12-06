install.packages("readxl")


quality_dimensions_table_1 <- readxl::read_xlsx("//NDATA9/daviss1$/My Documents/Completed QIPs/UPDATED_Quality_Improvement_Plan_BPI.xlsx", sheet = "Quality Risk 1" , range = "B5:C10")
quality_dimensions_table_2 <- readxl::read_xlsx("//NDATA9/daviss1$/My Documents/Completed QIPs/UPDATED_Quality_Improvement_Plan_BPI.xlsx", sheet = "Quality Risk 2" , range = "B5:C10")

key_metrics_m1 <- readxl::read_xlsx("//NDATA9/daviss1$/My Documents/Completed QIPs/UPDATED_Quality_Improvement_Plan_BPI.xlsx", sheet = "Key Metrics" , range = "B4:P8")
key_metrics_m2 <- readxl::read_xlsx("//NDATA9/daviss1$/My Documents/Completed QIPs/UPDATED_Quality_Improvement_Plan_BPI.xlsx", sheet = "Key Metrics" , range = "B10:P14")
key_metrics_m3 <- readxl::read_xlsx("//NDATA9/daviss1$/My Documents/Completed QIPs/UPDATED_Quality_Improvement_Plan_BPI.xlsx", sheet = "Key Metrics" , range = "B17:P21")
key_metrics_m4 <- readxl::read_xlsx("//NDATA9/daviss1$/My Documents/Completed QIPs/UPDATED_Quality_Improvement_Plan_BPI.xlsx", sheet = "Key Metrics" , range = "B24:P28")

progress_check_q1 <- readxl::read_xlsx("//NDATA9/daviss1$/My Documents/Completed QIPs/UPDATED_Quality_Improvement_Plan_BPI.xlsx", sheet = "Key Metrics" , range = "B4:P10")
progress_check_q2 <- readxl::read_xlsx("//NDATA9/daviss1$/My Documents/Completed QIPs/UPDATED_Quality_Improvement_Plan_BPI.xlsx", sheet = "Key Metrics" , range = "B13:P19")
progress_check_q3 <- readxl::read_xlsx("//NDATA9/daviss1$/My Documents/Completed QIPs/UPDATED_Quality_Improvement_Plan_BPI.xlsx", sheet = "Key Metrics" , range = "B22:P28")
progress_check_q4 <- readxl::read_xlsx("//NDATA9/daviss1$/My Documents/Completed QIPs/UPDATED_Quality_Improvement_Plan_BPI.xlsx", sheet = "Key Metrics" , range = "B31:P37")
progress_check_q5 <- readxl::read_xlsx("//NDATA9/daviss1$/My Documents/Completed QIPs/UPDATED_Quality_Improvement_Plan_BPI.xlsx", sheet = "Key Metrics" , range = "B40:P46")
