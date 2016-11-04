######################
#Bayes spam classifier
#2016.03.28
#by Tan Linuo
######################

data <- read.csv("train.csv")
spam <- data[data$is_spam == 1,]
normal <- data[data$is_spam == 0,]

len <- dim(data)[2] - 4
n_spam <- dim(spam)[1]
n_normal <- dim(normal)[1]
n_sample <- dim(data)[1]

p_spam <- (n_spam + 1) / (n_sample + 2)
p_normal <- (n_normal + 1) / (n_sample + 2)

#turn real value into bool value
spam <- spam[,1:len] != 0
normal <- normal[,1:len] != 0

x_spam <- (colSums(spam) + 1)  / (n_spam + 2)
x_normal <- (colSums(normal) + 1) / (n_normal + 2)

########
##Test##
########

test <- read.csv("test.csv")
n <- dim(test)[1] #test sample numbers

#turn real value into bool value
test_bool <- test[,1:len] != 0

#calculate bayes probability separately for each test sample(row)
#and write the judgement into result[]
result <- vector(length = n)
result[] <- 0

for (i in 1:n)
{
	index <- test_bool[i,] != 0
	p_data_spam <- prod(x_spam[index])
	p_data_normal <- prod(x_normal[index])

	#no divison of p(D)
	pr_spam <- p_spam * p_data_spam
	pr_normal <- p_normal * p_data_normal
	if (pr_spam > pr_normal) 
	{
		result[i] <- 1
	}
}

##########
#Accuracy#
##########
accu <- result == test[,dim(test)[2]]
p_accu <- sum(accu) * 1.0 / n
