library(ggplot2)

#working directory will depend on your computer
#setwd("C:/school/spring 2017/101C/project")
#setwd("~/Desktop/UCLA/2017Spring/101c/101cFinal")

#load data
data_train <- read.csv("lafdtraining.csv")

#data_train_clean will be the name for our cleaned data (all na removed)
data_train_clean <- na.omit(data_train)
data_train_clean <- data_train_clean[data_train_clean$elapsed_time > 0, ]

#distribution of elapsed_time
hist(data_train_clean$elapsed_time)
boxplot(data_train_clean$elapsed_time)


