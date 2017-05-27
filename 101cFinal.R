library(ggplot2)
library(chron)
library(dplyr)
setwd("C:/school/spring 2017/101C/project")

data_train <- read.csv("lafdtraining.csv")
data_train <- na.omit(data_train)

#change creation time to hours since midnight
data_train$Incident.Creation.Time..GMT. <- 24 * as.numeric(times(data_train$Incident.Creation.Time..GMT.))

#add new variable that groups creation times into 4 hour factors
data_train <- mutate(data_train, time_group = as.factor(floor(data_train$Incident.Creation.Time..GMT./3))) #divide by whatever interval you want

#rename variables
colnames(data_train) <- c("row", "ID", "year", "district", "emergency_code", "dispatch_seq", "dispatch_type", "unit_type", "PPE_level", "creation_time", "y", "time_group")

#change year, district, dispatch_seq to factor
data_train$dispatch_seq <- as.character(data_train$dispatch_seq)

data_train$district <- as.character(data_train$district)

data_train$year <- as.character(data_train$year)

#delete row, ID, emergency_code, creation_time
data_train_1 <- data_train[, -c(1,2,5,10)]

#set seed to your ID
set.seed(404318564)

#sample
x <- sample(1:2315060, 50000)

#create subset
data_subset <- data_train_1[x, ]

m1 <- lm(data = data_subset, y ~ .)


