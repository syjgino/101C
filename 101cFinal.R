library(ggplot2)
library(chron)
library(dplyr)
#working directory will depend on your computer
#setwd("C:/school/spring 2017/101C/project")
#setwd("~/Desktop/UCLA/2017Spring/101c/101cFinal")

raw_data <- read.csv("lafdtraining.csv")

cleaned_data <- raw_data

#change creation time to hours since midnight
cleaned_data$Incident.Creation.Time..GMT. <- 24 * as.numeric(times(cleaned_data$Incident.Creation.Time..GMT.))

#add new variable that groups creation times into 4 hour factors
cleaned_data <- mutate(cleaned_data, time_group = as.factor(floor(cleaned_data$Incident.Creation.Time..GMT.))) #divide by whatever interval you want

colnames(cleaned_data) <- c("row", "ID", "year", "district", "emergency_code", "dispatch_seq", "dispatch_type", "unit_type", "PPE_level", "creation_time", "y", "time_group")

#remove rows with NA response
cleaned_data <- cleaned_data[!is.na(cleaned_data$y),]

#change ID, year, district, and dispatch_seq to factor
cleaned_data$ID <- as.character(cleaned_data$ID)
cleaned_data$ID <- as.factor(cleaned_data$ID)

cleaned_data$year <- as.character(cleaned_data$year)
cleaned_data$year <- as.factor(cleaned_data$year)

cleaned_data$district <- as.character(cleaned_data$district)

cleaned_data$dispatch_seq <- as.character(cleaned_data$dispatch_seq)
cleaned_data$dispatch_seq <- as.factor(cleaned_data$dispatch_seq)


#delete row, emergency_code, creation_time
cleaned_data <- cleaned_data[, -c(1, 5,10)]

#use knnimpute to fill in missing na values
library(VIM)
#cleaned_data_impute <- kNN(data = cleaned_data, variable = c("dispatch_seq", "PPE_level"), k = 2)

#cleaned_data_mean <- cleaned_data
#cleaned_data_mean$dispatch_seq <- 

#temp_data just omits all na values
temp_data <- na.omit(cleaned_data)



testing_data <- read.table("testing.without.response", sep= ",", header = T)

#change creation time to hours since midnight
testing_data$Incident.Creation.Time..GMT. <- 24 * as.numeric(times(testing_data$Incident.Creation.Time..GMT.))

#add new variable that groups creation times into 4 hour factors
testing_data <- mutate(testing_data, time_group = as.factor(floor(testing_data$Incident.Creation.Time..GMT.))) #divide by whatever interval you want

colnames(testing_data) <- c("row", "ID", "year", "district", "emergency_code", "dispatch_seq", "dispatch_type", "unit_type", "PPE_level", "creation_time", "time_group")

#change ID, year, district, and dispatch_seq to factor
testing_data$ID <- as.character(testing_data$ID)
testing_data$ID <- as.factor(testing_data$ID)

testing_data$year <- as.character(testing_data$year)
testing_data$year <- as.factor(testing_data$year)

testing_data$district <- as.character(testing_data$district)

testing_data$dispatch_seq <- as.character(testing_data$dispatch_seq)
testing_data$dispatch_seq <- as.factor(testing_data$dispatch_seq)

#set seed to your ID
set.seed(404318564)

#sample
x <- sample(1:2315060, 500000)

#create subset
data_subset <- data_train_1[x, ]

m1 <- lm(data = data_subset, y ~ .)


#plot creation time vs y
ggplot()+
    geom_boxplot(data = data_subset, aes(y,time_group))

#xgb
library(xgboost)
xg1 = xgboost(data = data.matrix(data_subset[,c(1,2,3,4,5,6,7,9)]), label = data.matrix(data_subset[,8]), nrounds = 100, verbose = 1, early_stopping_rounds = 20, eta = .1, print_every_n=20)

#prediction
#clean test data
data_test_clean=na.omit(data_test)

#change creation time to hours since midnight
data_test_clean$Incident.Creation.Time..GMT. <- 24 * as.numeric(times(data_test_clean$Incident.Creation.Time..GMT.))

#add new variable that groups creation times into 4 hour factors
data_test_clean <- mutate(data_test_clean, time_group = as.factor(floor(data_test_clean$Incident.Creation.Time..GMT./1))) #divide by whatever interval you want

#rename variables
colnames(data_test_clean) <- c("row", "ID", "year", "district", "emergency_code", "dispatch_seq", "dispatch_type", "unit_type", "PPE_level", "creation_time", "time_group")

#change year, district, dispatch_seq to factor
data_test_clean$dispatch_seq <- as.character(data_test_clean$dispatch_seq)

data_test_clean$district <- as.character(data_test_clean$district)

data_test_clean$year <- as.character(data_test_clean$year)

#delete row, ID, emergency_code, creation_time
data_test_1 <- data_test_clean[, -c(1,2,5)]

#
preds.xgb.test = predict(xg1, newdata = data.matrix(data_test_1))
