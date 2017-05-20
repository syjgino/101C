library(ggplot2)

#working directory will depend on your computer
#setwd("C:/school/spring 2017/101C/project")

#load data
data_train <- read.csv("lafdtraining.csv")

#data_train_clean will be the name for our cleaned data(all na removed)
data_train_clean <- na.omit(data_train)

#distribution of elapsed_time
hist(data_train$elapsed_time)
boxplot(data_train$elapsed_time)
