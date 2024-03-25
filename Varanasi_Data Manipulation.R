
library("here")
library("tidyverse")
library("forcats")
library("dplyr")
library("table1")
library("aod")
library("readxl")

setwd("C:/Users/msalvat4/Documents/Varanasi/Data")

#LOADING DATA
Varanasi <- read_xlsx("Varanasi.xlsx")
data1 <- Varanasi

#DATA MANIPULATION

##filtering out one missing entry for HCV and HIV status (now the name of the data of the dataset is data_filtered)
data_filtered <- data1 %>% filter(!is.na(hcvrapid))
data_filtered <- data1 %>% filter(!is.na(hiv1rapid))

##filtering out site variable (since they are all Varanasi) 
data_filtered <- subset(data_filtered, select = -site)

##filtering out all females from the dataset
data_filtered <- data_filtered[data_filtered$gender != "Female", ]

##labeling some demographic variables
names(data_filtered)[names(data_filtered) == "cm_age"] <- "age"
names(data_filtered)[names(data_filtered) == "dg4"] <- "religion"
names(data_filtered)[names(data_filtered) == "dg5"] <- "relationship"
names(data_filtered)[names(data_filtered) == "dg6"] <- "education"
names(data_filtered)[names(data_filtered) == "dg7"] <- "earnings"
names(data_filtered)[names(data_filtered) == "screen10"] <- "incarceration"
names(data_filtered)[names(data_filtered) == "dg10"] <- "homelessness"
names(data_filtered)[names(data_filtered) == "hiv1rapid"] <- "hiv"
names(data_filtered)[names(data_filtered) == "hcvrapid"] <- "hcv"

##making variables categorical type
data_filtered$religion <- as.factor(data_filtered$religion)
data_filtered$relationship <- as.factor(data_filtered$relationship)
data_filtered$education <- as.factor(data_filtered$education)
data_filtered$earnings <- as.factor(data_filtered$earnings)
data_filtered$incarceration <- as.factor(data_filtered$incarceration)
data_filtered$homelessness <- as.factor(data_filtered$homelessness)
data_filtered$hcv <- as.factor(data_filtered$hcv)
data_filtered$hiv <- as.factor(data_filtered$hiv)
data_filtered$su50a <- as.factor(data_filtered$su50a)
data_filtered$su35 <- as.factor(data_filtered$su35)
data_filtered$su36 <- as.factor(data_filtered$su36)

##rename some of the categorical variables into more descriptive names
data_filtered$incarceration <- factor(data_filtered$incarceration, levels = c("0", "1"), labels = c("not incarcerated", "incarcerated"))
data_filtered$hiv <- factor(data_filtered$hiv, levels = c("0", "1"), labels = c("negative", "positive"))
data_filtered$hcv <- factor(data_filtered$hcv, levels = c("0", "1"), labels = c("negative", "positive"))

##grouping categorical variables and establishing reference levels for comparison
data_filtered$relationship <- fct_collapse(data_filtered$relationship,
                                           inrelationship = c("1", "2", "3"),
                                           norelationship = c("4", "5", "6"),
                                           nevermarried = c("7")
)

data_filtered$education <- fct_collapse(data_filtered$education,
                                           noschooling = c("1"),
                                           primaryschool = c("2"),
                                           postprimaryyschool = c("3", "4", "5", "6", "7", "8"),
)

data_filtered$earnings <- fct_collapse(data_filtered$earnings,
                                        monthlyorseasonalwages = c("1", "4"),
                                        weeklywages = c("2"),
                                        dailywages = c("3"),
                                        noearnings = c("5", "6", "7", "8", "9", "10", "11"),
)
data_filtered$earnings <- relevel(data_filtered$earnings, ref = "noearnings")

data_filtered$homelessness <- fct_collapse(data_filtered$homelessness,
                                       Yes = c("8"),
                                       No = c("1", "2", "3", "4", "5", "6", "7", "10"),
)

##grouping age variable in ranges
data_filtered$age <- as.numeric(as.character(data_filtered$age))

data_filtered$age = cut(data_filtered$age,
                           breaks = c(-Inf, 25, 35, 45, 55, Inf),
                           labels = c("16-25", "26-35", "36-45", "46-55", "56+"),
                           right = FALSE)

##grouping number of women sex partners, and dropping Dont know - 997 values)
data_filtered <- data_filtered %>%
  mutate(wsexnum = case_when(
    su50a %in% c("5", "6", "7", "8", "9", "10", "12", "15","20", "25","70","100", "977", "995") ~ ">5",
    TRUE ~ as.character(su50a)  # Keep other categories unchanged
  ))

data_filtered$wsexnum[data_filtered$wsexnum == "997"] <- NA
data_filtered$wsexnum[data_filtered$wsexnum == "995"] <- NA

###Converting 1 as the reference category
data_filtered$wsexnum <- factor(data_filtered$wsexnum, levels = c("1", "2", "3", "4", ">5"))


##grouping su35 and su36 in a single variable answering overall needle sharing, that is Yes (1) if there is a Yes in either su35 or su36
data_filtered$shared_needle <- ifelse(data_filtered$su35 == 1 | data_filtered$su36 == 1, 1, 0)
data_filtered$shared_needle <- as.factor(data_filtered$shared_needle)

data_filtered$shared_needle <- factor(data_filtered$shared_needle, levels = c("0", "1"), labels = c("No", "Yes"))


##grouping su58 and su59 in a single variable answering any type of participation in transactional sex
data_filtered$transactional_sex <- ifelse(data_filtered$su58 == 1 | data_filtered$su59 == 1, 1, 0)
data_filtered$transactional_sex <- as.factor(data_filtered$transactional_sex)

data_filtered$transactional_sex <- factor(data_filtered$transactional_sex, levels = c("0", "1"), labels = c("No", "Yes"))


table(data_filtered$sv13)


##condom use
names(data_filtered)[names(data_filtered) == "sv13"] <- "condom_use"
data_filtered$condom_use <- as.factor(data_filtered$condom_use)
data_filtered$condom_use <- fct_collapse(data_filtered$condom_use,
                                           No = c("1"),
                                           Yes = c("2", "3", "4", "5"),
                                           Other = c("996", "997")
)


##Create a composite score for PHQ9 (excluding column phq10)
calculate_phq9_score <- function(data_filtered, exclude_column) {
  phq9_items <- data_filtered[, grep("phq", names(data_filtered))]
  phq9_items1 <- phq9_items[, !names(phq9_items) %in% exclude_column]
  data_filtered$phq9_score <- rowSums(phq9_items1, na.rm = TRUE)
  return(data_filtered)
}
data_filtered2 <- calculate_phq9_score(data_filtered, exclude_column = "phq10")

###Creating a binary variable 'depressed' based on a PHQ9 cutoff of 10. It adds a new column in the dataset indicating 1 when 10 or above, and 0 for less than 10
data_filtered2$depressed <- ifelse(data_filtered2$phq9_score >= 10, 1, 0)
## make factor
data_filtered2$depressed <- as.factor(data_filtered2$depressed)

data_filtered2$depressed <- factor(data_filtered2$depressed, levels = c("0", "1"), labels = c("No", "Yes"))

##Create a composite score for AUDIT-Alcohol Use
calculate_audit_score <- function(data_filtered) {
  audit_columns <- c("su25", "su26", "su27", "su28", "su29", "su30", "su31", "su32", "su33", "su34")
  audit_score <- data_filtered[, audit_columns]
  data_filtered$audit_score <- rowSums(audit_score, na.rm = TRUE)
  return(data_filtered)
}
data_filtered2 <- calculate_audit_score(data_filtered2)

#Creating a binary variable 'at_risk_alcohol_use' based on an AUDIT cutof of 8. It adds a new column in the data set indicating 1 for 8 or above, and 0 for less than 8
data_filtered2$at_risk_alcohol_use <- ifelse(data_filtered2$audit_score >= 8, 1, 0)

## make factor
data_filtered2$at_risk_alcohol_use <- as.factor(data_filtered2$at_risk_alcohol_use)

data_filtered2$at_risk_alcohol_use <- factor(data_filtered2$at_risk_alcohol_use, levels = c("0", "1"), labels = c("No", "Yes"))


#Table 1 with hcv
table1(~ age + relationship + education + earnings + incarceration + homelessness + hiv + wsexnum + shared_needle + depressed + at_risk_alcohol_use + transactional_sex| hcv, data = data_filtered2)

#Table 1 with hiv
table1(~ age + relationship + education + earnings + incarceration + homelessness + hcv + wsexnum + shared_needle + depressed + at_risk_alcohol_use + transactional_sex + condom_use| hiv, data = data_filtered2)




