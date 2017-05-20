
library(ggplot2)

#working directory will depend on your computer
setwd("C:/school/spring 2017/101C/project")

#load data
data_train <- read.csv("lafdtraining.csv")

#data_train_clean will be the name for our cleaned data
data_train_clean <- na.omit(data_train)
