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

##Create a composite score for PHQ9 (excluding column phq10)
calculate_phq9_score <- function(data_filtered, exclude_column) {
  phq9_items <- data_filtered[, grep("phq", names(data_filtered))]
  phq9_items1 <- phq9_items[, !names(phq9_items) %in% exclude_column]
  data_filtered$phq9_score <- rowSums(phq9_items1, na.rm = TRUE)
  return(data_filtered)
}
data_filtered2 <- calculate_phq9_score(data_filtered, exclude_column = "phq10")
