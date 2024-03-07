
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

##filtering out one missing entry for HCV status (now the name of the data of the dataset is data_filtered)
data_filtered <- data1 %>% filter(!is.na(hcvrapid))

##filtering out site variable (since they are all Varanasi) 
data_filtered1 <- subset(data_filtered, select = -site)

##filtering out all females from the dataset
data_filtered2 <- data_filtered1[data_filtered1$gender != "Female", ]

##labelling some demographic variables
names(data_filtered2)[names(data_filtered2) == "dg5"] <- "relationship"
names(data_filtered2)[names(data_filtered2) == "dg6"] <- "education"
names(data_filtered2)[names(data_filtered2) == "dg7"] <- "earnings"
names(data_filtered2)[names(data_filtered2) == "screen10"] <- "incarceration"
names(data_filtered2)[names(data_filtered2) == "dg10"] <- "homelessness"

##making variables categorical type
data_filtered2$relationship <- as.factor(data_filtered2$relationship)
data_filtered2$education <- as.factor(data_filtered2$education)
data_filtered2$earnings <- as.factor(data_filtered2$earnings)
data_filtered2$incarceration <- as.factor(data_filtered2$incarceration)
data_filtered2$homelessness <- as.factor(data_filtered2$homelessness)
data_filtered2$hcvrapid <- as.factor(data_filtered2$hcvrapid)
data_filtered2$hiv1rapid <- as.factor(data_filtered2$hiv1rapid)

##grouping categorical variable responses to simplify interpretation
data_filtered2$relationship <- fct_collapse(data_filtered2$relationship,
                                           inrelationship = c("1", "2", "3"),
                                           norelationship = c("4", "5", "6"),
                                           nevermarried = c("7")
)

data_filtered2$education <- fct_collapse(data_filtered2$education,
                                           noschooling = c("1"),
                                           primaryschool = c("2"),
                                           secondaryschool = c("3"),
                                           highschool = c("4"),
                                           posthighschool = c("5", "6", "7", "8"),
)

data_filtered2$earnings <- fct_collapse(data_filtered2$earnings,
                                        monthlywages = c("1"),
                                        weeklywages = c("2"),
                                        dailywages = c("3"),
                                        seasonalwages = c("4"),
                                        noearnings = c("5", "6", "7", "8", "9", "10", "11"),
)

data_filtered2$homelessness <- fct_collapse(data_filtered2$homelessness,
                                       Yes = c("8"),
                                       No = c("1", "2", "3", "4", "5", "6", "7", "10"),
)


##grouping age variable in ranges
data_filtered2$cm_age = cut(data_filtered2$cm_age,
                              breaks = c(-Inf, 15, 25, 35, 45, 55, 65, Inf),
                              labels = c("0-15", "16-25", "26-35", "36-45", "46-55", "56-65", "66+"),
                              right = FALSE)

##Create a composite score for PHQ9 (excluding column phq10)
calculate_phq9_score <- function(data_filtered2, exclude_column) {
  phq9_items <- data_filtered2[, grep("phq", names(data_filtered2))]
  phq9_items1 <- phq9_items[, !names(phq9_items) %in% exclude_column]
  data_filtered2$phq9_score <- rowSums(phq9_items1, na.rm = TRUE)
  return(data_filtered2)
}
data_filtered3 <- calculate_phq9_score(data_filtered2, exclude_column = "phq10")

##Create a composite score for AUDIT-Alcohol Use
calculate_audit_score <- function(data_filtered3) {
  audit_columns <- c("su25", "su26", "su27", "su28", "su29", "su30", "su31", "su32", "su33", "su34")
  audit_score <- data_filtered3[, audit_columns]
  data_filtered3$audit_score <- rowSums(audit_score, na.rm = TRUE)
  return(data_filtered3)
}
data_filtered4 <- calculate_audit_score(data_filtered3)

##Creating a binary variable 'depressed' based on a PHQ9 cutoff of 10. It adds a new column in the dataset indicating 1 when 10 or above, and 0 for less than 10
data_filtered4$depressed <- ifelse(data_filtered4$phq9_score >= 10, 1, 0)

##Creating a binary variable 'at_risk_alcohol_use' based on an AUDIT cutof of 8. It adds a new column in the data set indicating 1 for 8 or above, and 0 for less than 8
data_filtered4$at_risk_alcohol_use <- ifelse(data_filtered4$audit_score >= 8, 1, 0)

##Combining su35 and su36 in a single variable answering overall needle sharing, that is Yes (1) if there is a Yes in either su35 or su36
data_filtered4$shared_needle <- ifelse(data_filtered4$su35 == 1 | data_filtered4$su36 == 1, 1, 0)

#Making Table 1
table1(~ cm_age + relationship + education + earnings + incarceration + homelessness | hcvrapid, data = data_filtered4)


#LOGISTIC REGRESSION

#Model 1: Only with demograpghics we have agreed
logit1 <- glm(hcvrapid ~ cm_age + relationship + education + earnings + incarceration + homelessness, data=data_filtered4, family = "binomial")
summary(logit1)

#Model 2: Demographics, depression, alcohol use, hivprevalence
logit2 <- glm(hcvrapid ~ cm_age + relationship + education + earnings + incarceration + homelessness + depressed + at_risk_alcohol_use + hiv1rapid, data=data_filtered4, family = "binomial")
summary(logit2)

#Model 3: Demopraphics, depression, alhocol use, hivprevalence, shared needles, sexual partners, age of injection




