library("here")
library("tidyverse")
library("dplyr")
library("table1")
library("aod")
library("readxl")

setwd("C:/Users/msalvat4/Documents/Varanasi/Data")

#Load data
Varanasi <- read_xlsx("Varanasi.xlsx")
data1 <- Varanasi

#Data manipulation

##filter out one missing entry for HCV status (now the name of the data of the dataset is data_filtered)
data_filtered <- data1 %>% filter(!is.na(hcvrapid))

###filter out site variable (since they are all Varanasi) 
data_filtered <- subset(data_filtered, select = -site)

##Create a composite score for AUDIT-Alcohol Use
calculate_audit_score <- function(data_filtered) {
  audit_columns <- c("su25", "su26", "su27", "su28", "su29", "su30", "su31", "su32", "su33", "su34")
  audit_score <- data_filtered[, audit_columns]
  data_filtered$audit_score <- rowSums(audit_score, na.rm = TRUE)
  return(data_filtered)
}
data_filtered3 <- calculate_audit_score(data_filtered)

