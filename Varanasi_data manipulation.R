
library("here")
library("tidyverse")
library("forcats")
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
data_filtered1 <- data_filtered[data_filtered$gender != "Female", ]


##filter out site variable (since they are all Varanasi) 
data_filtered <- subset(data_filtered, select = -site)

##labeling some demographic variables
names(data_filtered)[names(data_filtered) == "dg5"] <- "relationship"
names(data_filtered)[names(data_filtered) == "dg6"] <- "education"
names(data_filtered)[names(data_filtered) == "dg7"] <- "earnings"
names(data_filtered)[names(data_filtered) == "screen10"] <- "incarceration"
names(data_filtered)[names(data_filtered) == "dg10"] <- "homelessness"

##make variables categorical type
data_filtered$relationship <- as.factor(data_filtered$relationship)
data_filtered$education <- as.factor(data_filtered$education)
data_filtered$earnings <- as.factor(data_filtered$earnings)
data_filtered$homelessness <- as.factor(data_filtered$homelessness)
data_filtered$incarceration <- as.factor(data_filtered$incarceration)
data_filtered$hcvrapid <- as.factor(data_filtered$hcvrapid)
data_filtered$hiv1rapid <- as.factor(data_filtered$hiv1rapid)


##grouping categorical variable responses to simplify interpretation
data_filtered$relationship <- fct_collapse(data_filtered$relationship,
                                           inrelationship = c("1", "2", "3"),
                                           norelationship = c("4", "5", "6"),
                                           nevermarried = c("7")
)

data_filtered$education <- fct_collapse(data_filtered$education,
                                           noschooling = c("1"),
                                           primaryschool = c("2"),
                                           secondaryschool = c("3"),
                                           highschool = c("4"),
                                           posthighschool = c("5", "6", "7", "8"),
)

data_filtered$earnings <- fct_collapse(data_filtered$earnings,
                                        monthlywages = c("1"),
                                        weeklywages = c("2"),
                                        dailywages = c("3"),
                                        seasonalwages = c("4"),
                                        noearnings = c("5", "6", "7", "8", "9", "10", "11"),
)

data_filtered$homelessness <- fct_collapse(data_filtered$homelessness,
                                       Yes = c("8"),
                                       No = c("1", "2", "3", "4", "5", "6", "7", "10"),
)


##grouping age variable in ranges
data_filtered$cm_age = cut(data_filtered$cm_age,
                              breaks = c(-Inf, 15, 25, 35, 45, 55, 65, Inf),
                              labels = c("0-15", "16-25", "26-35", "36-45", "46-55", "56-65", "66+"),
                              right = FALSE)















