
####################
#linear regression
#2016.03.17
#by Tan Linuo
####################

#data preparation
#extract y vector and x matrix from train.csv data
train <- read.csv("train.csv")
y <- train$price
x_ori <-train$sqft_living
n <- length(y)
x <- cbind(rep(1,each = n) ,x_ori) #rep 1 for n times for Theta0

#extract y_test vector and x_test matrix from test.csv data
test <-read.csv("test.csv")
y_test <-test$price
x_test_ori <-test$sqft_living
n_test <-length(y_test)
x_test <- cbind(rep(1,each = n_test), x_test_ori)

####################
#1. normal equation#
####################

#using normal equation to get the Theta vector
Theta_1 <- solve( t(x) %*% x ) %*% t(x) %*% y

#            [,1]
#[1,] -49800.4068
#[2,]    283.8428

#using Theta to get the prediction of y (price)
y_pre_1 <- x %*% Theta_1

#plot the train data and the fitting line
plot(x_ori, y, main = "Linear Regression by Normal Equation", xlab = "sqft_living", ylab = "price")
lines(x_ori, y_pre_1, col = "red")

#using test.csv to calculate RMSE
y_pre_test_1 <- x_test %*% Theta_1
RMSE_1 <-sqrt((sum((y_pre_test_1 - y_test)^2))/n_test)

#RMSE = 264054.5


#####################
#2. Gradient descent#
#####################
#set parameters
Theta_2 <- c(0,0) 
Alpha <- 0.0000001

#iterate for t times
t <- 1000
for(j in 1:t)
{
	y_pre_2 <- x %*% Theta_2 
	for(i in 1:length(Theta_2))
	{
		Theta_2[i] <- Theta_2[i] + Alpha * sum((y - y_pre_2) * x[,i] ) / n	
	}
}

#using Theta to get the prediction of y (price)
Theta_2
y_pre_2 <- x %*% Theta_2

#using test.csv to calculate RMSE
y_pre_test_2 <- x_test %*% Theta_2
RMSE_2 <-sqrt((sum((y_pre_test_2 - y_test)^2))/n_test)
RMSE_2

#plot the train data and the fitting line
plot(x_ori, y, main = "Linear Regression by Gradient Descent", xlab = "sqft_living", ylab = "price")
lines(x_ori, y_pre_2, col = "red")



###################
#3.Newton's method#
###################
Theta_3 <- c(1,1)
y_pre_3 <- x %*% Theta_3
#the derivarion, didn't find proper function in R to ahchieve it
H[1,1]<- 1
H[1,2]<- sum(x[,2])/n
H[2,1]<- sum(x[,2])/n
H[2,2]<- sum(x[,2]^2)/n

#iterate for t times, t required is much smaller than in Gradient descent
t <- 10
for(j in 1:t)
{
	y_pre_3 <- x %*% Theta_3
	J<-c(sum(y_pre_3-y)/n, sum(x[,2]*(y_pre_3-y))/n)
	Theta_3 <- Theta_3 - solve(H) %*% J	
}

#using Theta to get the prediction of y (price)
#Theta_3
#            [,1]
#[1,] -49800.4068
#[2,]    283.8428
y_pre_3 <- x %*% Theta_3

#using test.csv to calculate RMSE
y_pre_test_3 <- x_test %*% Theta_3
RMSE_3 <-sqrt((sum((y_pre_test_3 - y_test)^2))/n_test)
#RMSE_3 = 264054.5

#plot the train data and the fitting line
plot(x_ori, y, main = "Linear Regression by Newton's method", xlab = "sqft_living", ylab = "price")
lines(x_ori, y_pre_3, col = "red")