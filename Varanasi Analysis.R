install.packages("blorr")

library("here")
library("tidyverse")
library("dplyr")
library("table1")
library("aod")
library("readxl")
library("blorr")

setwd("C:/Users/msalvat4/Documents/Varanasi/Data")


#Load data
Varanasi <- read_xlsx("Varanasi.xlsx")
data1 <- Varanasi


#Explore data (now the name of the dataset is data1)
glimpse(data1)
table(data1$gender)
table(data1$seed)
table(data1$hcvrapid)
hist(data1$cm_age)


#Data manipulation

##filter out one missing entry for HCV status (now the name of the data of the dataset is data_filtered)
data_filtered <- data1 %>% filter(!is.na(hcvrapid))

###filter out site variable (since they are all Varanasi) 
data_filtered <- subset(data_filtered, select = -site)

##labeling some demographic variables
names(data_filtered)[names(data_filtered) == "screen4"] <- "age"
names(data_filtered)[names(data_filtered) == "dg4"] <- "religion"
names(data_filtered)[names(data_filtered) == "dg5"] <- "relationship"
names(data_filtered)[names(data_filtered) == "dg6"] <- "education"
names(data_filtered)[names(data_filtered) == "dg7"] <- "earnings"

table(data_filtered$relationship)

##make variables categorical type
data_filtered$relationship <- as.factor(data_filtered$relationship)
type_sum(data_filtered$relationship)
summary(data_filtered$relationship)

data_filtered$religion <- as.factor(data_filtered$religion)
summary(data_filtered$religion)

data_filtered$education <- as.factor(data_filtered$education)
summary(data_filtered$education)

data_filtered$earnings <- as.factor(data_filtered$earnings)
summary(data_filtered$earnings)

data_filtered$earnings <- as.factor(data_filtered$earnings)
summary(data_filtered$earnings)

data_filtered$hcvrapid <- as.factor(data_filtered$hcvrapid)
summary(data_filtered$hcvrapid)

##Change the numeric value to categorical names
religion_levels <- c("Hinduism", "Christianity", "Sikhism", "Buddhism", "Jainism", "Islam", "Atheist", "Parsi", "Other")  
data_filtered$religion <- factor(data_filtered$religion, levels = 1:9, labels = religion_levels)

relationship_levels <- c("Married", "Living with partner, but not married", "In a long-term relationship, but not living with partner", "Widowed", "Divorced", "Separated from partner but still married", "Never married")  
data_filtered$relationship <- factor(data_filtered$relationship, levels = 1:7, labels = relationship_levels)

education_levels <- c("No schooling", "Primary school", "Secondary school", "High school/graduate", "Vocational or trade school", "College or university, not complete", "College or university, complete", "Post-graduate")  
data_filtered$education <- factor(data_filtered$education, levels = 1:8, labels = education_levels)

earnings_levels <- c("Monthly wages", "Weekly wages", "Daily wages", "Seasonally/intermittently employed", "Temporarily laid off, sick leave", "Unemployed, looking for work", "Unemployed, not looking for work", "Retired", "Disabled, permanently or temporarily", "Partner takes care of me / homemaker", "Student")  
data_filtered$earnings <- factor(data_filtered$earnings, levels = 1:11, labels = earnings_levels)

hcvrapid_levels <- c("Negative", "Positive")
data_filtered$hcvrapid <- factor(data_filtered$hcvrapid, levels = 0:1, labels = hcvrapid_levels)

##Create a composite score for PHQ9 (excluding column phq10)
calculate_phq9_score <- function(data_filtered, exclude_column) {
  phq9_items <- data_filtered[, grep("phq", names(data_filtered))]
  phq9_items1 <- phq9_items[, !names(phq9_items) %in% exclude_column]
  data_filtered$phq9_score <- rowSums(phq9_items1, na.rm = TRUE)
  return(data_filtered)
}
data_filtered2 <- calculate_phq9_score(data_filtered, exclude_column = "phq10")

##Create a composite score for AUDIT-Alcohol Use
calculate_audit_score <- function(data_filtered2) {
  audit_columns <- c("su25", "su26", "su27", "su28", "su29", "su30", "su31", "su32", "su33", "su34")
  audit_score <- data_filtered2[, audit_columns]
  data_filtered2$audit_score <- rowSums(audit_score, na.rm = TRUE)
  return(data_filtered2)
}
data_filtered3 <- calculate_audit_score(data_filtered2)


#Making Table 1
table1(~ cm_age + gender + religion + education + relationship + earnings | hcvrapid, data = data_filtered3)


#Logistic Regression
testlogit <- glm(hcvrapid ~ cm_age + gender + religion + relationship + education + earnings, data=data_filtered3, family = "binomial")
summary(testlogit)


#Exponentiate to get OR - while holding other variables constant (each category is compared against the reference variable, which is the first one listed)
exp(coef(testlogit))


#Running univariate logistic regression for each variable with hcvrapid as "y" and each other variable as dependent variable. Some models did not converge, so I think the code below skips those. For variables with p values, prints list of variables with p-value < 0.1 in univariate logistic regression 
independent_variables <- names(data_filtered3)[names(data_filtered3) != "hcvrapid"]

logistic_models <- list()

max_iterations <- 1000

control_params <- list(maxit = max_iterations)

for (variable in independent_variables) {
  tryCatch({
    formula <- formula(paste("hcvrapid ~", variable))
    model <- glm(formula, data = data_filtered3, family = binomial, control = control_params)
    logistic_models[[variable]] <- summary(model)
  }, error = function(e) {
    cat("Error for variable:", variable, "\n")
    cat("Error message:", conditionMessage(e), "\n")
  })
}

significant_variables <- list()

significance_level <- 0.1

for (variable in independent_variables){
  tryCatch({
    if (variable %in% names(logistic_models)) {
      p_value <- logistic_models[[variable]]$coefficients[2, "Pr(>|z|)"]
      
      if (!is.na(p_value) && p_value < significance_level) {
        significant_variables[[variable]] <- p_value
      }
    }
  }, error = function(e) {
    cat("Error for variable:", variable, "\n")
    cat("Error message:", conditionMessage(e), "\n")
  })
}

print(significant_variables)


##Multivariate model with all variables which were significant in univariate model from demographics module
dglogit <- glm(hcvrapid ~ screen10 + relationship + earnings + dg8 + dg9 + dg13 + dg15, data=data_filtered3, family = "binomial")
summary(dglogit)

##Multivariate model with all variables which were significant in univariate model from network module
nwlogit <- glm(hcvrapid ~ nw1a + nw1h + nw2a + nw2 + nw3 + nw4 + nw9 + nw10 + nw11, data=data_filtered3, family = "binomial")
summary(nwlogit)

##Multivariate model based on past lit and significant variables which make sense
logit2 <- glm(hcvrapid ~ screen10 + cm_age + relationship + earnings + audit_score + phq9_score + su36 + su40aa + su49, data=data_filtered3, family = "binomial")
summary(logit2)
#I had to change variables 36, 40, and 49 to ask have you ever had injecting or sexual (anal/vaginal) partners? rather than in the last six months how many partners? The way the data is answered, there are many missing data points and the model does not work


#Testing goodnes of fit
blr_test_hosmer_lemeshow(logit2)
