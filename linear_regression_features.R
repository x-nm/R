#####################################
#linear regression with more features
#2016.03.17
#by Tan Linuo
#####################################

#data preparation
#extract y vector and x matrix from train.csv data
#features was chosen by plot and observations
train <- read.csv("train.csv")
data <- train[,c(3,6,13,20)]
y <- data$price
n <- length(y)
x <- cbind(rep(1,each = n) ,data[,2:4]) #rep 1 for n times for Theta0
x <- as.matrix(x)

#extract y_test vector and x_test matrix from test.csv data
test <-read.csv("test.csv")
data_test <- train[,c(3,6,13,20)]
y_test <-data_test$price
n_test <-length(y_test)
x_test <- cbind(rep(1,each = n_test), data_test[,2:4])
x_test <- as.matrix(x_test)

#################
#normal equation#
#################

#using normal equation to get the Theta vector
Theta <- solve( t(x) %*% x ) %*% t(x) %*% y

#using Theta to get the prediction of y (price)
y_pre <- x %*% Theta

#using test.csv to calculate RMSE
y_pre_test <- x_test %*% Theta
RMSE <-sqrt((sum((y_pre_test - y_test)^2))/n_test)

#Theta
#                          [,1]
#rep(1, each = n) -124175.25897
#sqft_living          253.35246
#sqft_above           -30.86332
#sqft_living15         96.72318
#RMSE
#[1] 257444.9 