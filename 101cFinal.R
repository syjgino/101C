library(ggplot2)
library(chron)
library(dplyr)

setwd("C:/school/spring 2017/101C/project")

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

row_id <- testing_data$row

testing_data <- testing_data[, -c(1, 5, 10)]

library(car)
temp_data$newdist=car::recode(temp_data$district, "c(85,38,36,101)='a' ; c(49,40)='b' ;  112='c' ;  79='d' ;  c(95,51,5,63,62)='e'; 67='f' ;  c(43,92,59,37,58)='g'; c(71,19)='h' ;  c(69,23)='i' ;  c(64,65,57,33,66,21,46)='j' ;  c(14,15,13,11,10,3,9,4,26)='k' ;  c(17, 25, 2, 34, 94, 68)='l' ;  c(29, 61,27,52,6,35,20)='m' ;  c(41,82)='n' ; c(56,44,16,50,55,12)='o' ; c(42,47)='p' ; c(76,97,99)='q' ;  c(86,60,78,89,102)='r' ;  c(108)='s'; c(109)='t'; c(74)='u' ; c(24,77)='v' ;  c(88,39,83,81,7,90,100,93,103,70,98)='w' ; c(73,87,75)='x' ;  c(84,105,96)='y' ; c(72,104,107)='z'; c(8,28,18,91)='aa'; else='NA'" )

temp_data$newdist[which(temp_data$newdist=="NA")] <- as.character(temp_data$district[which(temp_data$newdist=="NA")])
temp_data$newdist<-as.factor(temp_data$newdist)


#set seed to your ID
set.seed(404318564)

#sample
x <- sample(1:2315060, 1000000)

#create subset
data_subset <- temp_data[x, ]
remaining_data <- temp_data[-x,]

#XGBOOST
library(xgboost)
xg1 <- xgboost(data = data.matrix(data_subset[,c(1,2,3,4,5,6,7,9)]), label = data.matrix(data_subset[,8]), nrounds = 500, verbose = 1, early_stopping_rounds = 20, eta = 0.1, print_every_n=20, max_depth = 7, eval_metric = "rmse")

#ignore district, instead use grouping of districts
data_subset$dispatch_seq <- as.character(data_subset$dispatch_seq)
xg2 <- xgboost(data = data.matrix(data_subset[,c(1,2,4,5,6,7,9,10)]), label = data.matrix(data_subset[,8]), nrounds = 500, verbose = 1, early_stopping_rounds = 20, eta = 0.1, print_every_n=20, max_depth = 7, eval_metric = "rmse")

#use both districts and groups
xg3 <- xgboost(data = data.matrix(data_subset[,c(1,2,3,4,5,6,7,9,10)]), label = data.matrix(data_subset[,8]), nrounds = 500, verbose = 1, early_stopping_rounds = 20, eta = 0.1, print_every_n=20, max_depth = 7, eval_metric = "rmse")


cv_pred1 <- predict(xg1, newdata = data.matrix(remaining_data))
cv_pred2 <- predict(xg2, newdata = data.matrix(remaining_data))
cv_pred3 <- predict(xg3, newdata = data.matrix(remaining_data))

mse_cv1 <- mean((remaining_data$y - cv_pred1)^2)
mse_cv2 <- mean((remaining_data$y - cv_pred2)^2)
mse_cv3 <- mean((remaining_data$y - cv_pred3)^2)


a <- c(20000, 30000, 40000, 50000, 60000)
b <- seq(1.05, 2, 0.05)
c <- c()

for(i in 1:length(a)){
  for(j in 1:length(b)){
    inf <- cv_pred1
    inf[which(inf>a[1])] <- b[1] * inf[which(inf>a[1])]
    mse <- mean((remaining_data$y - inf)^2)
    c <- c(c,mse)
  }
}

min(c)

mse_inflate
mse_cv1
mse_cv2
mse_cv3

predictions <- predict(xg2, newdata = data.matrix(testing_data))

submission <- data.frame(row_id, predictions)
names(submission) <- c("row.id", "prediction")

write.csv(submission, file = "submission.csv", row.names = F)
