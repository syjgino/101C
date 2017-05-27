library(ggplot2)

#working directory will depend on your computer
setwd("C:/school/spring 2017/101C/project")

#load data
data_train <- read.csv("lafdtraining.csv")

#data_train_clean will be the name for our cleaned data (all na removed)
data_train_clean <- na.omit(data_train)
data_train_clean <- data_train[data_train_clean$elapsed_time > 0, ]

#change creation time to hours since midnight
data_train$Incident.Creation.Time..GMT. <- 24 * as.numeric(times(data_train$Incident.Creation.Time..GMT.))

#add new variable that groups creation times into 4 hour factors
data_train <- mutate(data_train, time_group = as.factor(floor(data_train$Incident.Creation.Time..GMT./4)))

#set seed to your ID
set.seed(404318564)

#sample
x <- sample(1:2315060, 50000)

#distribution of elapsed_time
hist(data_train_clean$elapsed_time)
boxplot(data_train_clean$elapsed_time)


