library(ggplot2)
library(chron)
library(dplyr)
#working directory will depend on your computer
#setwd("C:/school/spring 2017/101C/project")
#setwd("~/Desktop/UCLA/2017Spring/101c/101cFinal")

#load data
data_train <- read.csv("lafdtraining.csv")

#data_train_clean will be the name for our cleaned data (all na removed)
data_train_clean <- na.omit(data_train)

#change creation time to hours since midnight
data_train_clean$Incident.Creation.Time..GMT. <- 24 * as.numeric(times(data_train_clean$Incident.Creation.Time..GMT.))

#add new variable that groups creation times into 4 hour factors
data_train_clean <- mutate(data_train_clean, time_group = as.factor(floor(data_train_clean$Incident.Creation.Time..GMT./.5))) #divide by whatever interval you want

#rename variables
colnames(data_train_clean) <- c("row", "ID", "year", "district", "emergency_code", "dispatch_seq", "dispatch_type", "unit_type", "PPE_level", "creation_time", "y", "time_group")

#change year, district, dispatch_seq to factor
data_train_clean$dispatch_seq <- as.character(data_train_clean$dispatch_seq)

data_train_clean$district <- as.character(data_train_clean$district)

data_train_clean$year <- as.character(data_train_clean$year)

#delete row, ID, emergency_code, creation_time
data_train_1 <- data_train_clean[, -c(1,2,5)]

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
