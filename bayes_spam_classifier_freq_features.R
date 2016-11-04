#############################################
#Bayes spam classifier with feature choosing
#created 2016.04.02
#by Tan Linuo
#############################################

data <- read.csv("train.csv")
spam <- data[data$is_spam == 1,]
normal <- data[data$is_spam == 0,]

len <- dim(data)[2] - 4 #use only word_freq and char_freq as features
n_spam <- dim(spam)[1]
n_normal <- dim(normal)[1]
n_sample <- dim(data)[1]

p_spam <- (n_spam + 1) / (n_sample + 2)
p_normal <- (n_normal + 1) / (n_sample + 2)

#use word_freq to calculate the p(word|y)
#based on the data given, assume the length of each email to be 100,
#and the data given (word_freq(*100 already)) will then represent 
#count of the word in an email sample
spam <- spam[,1:len] 
normal <- normal[,1:len]

x_spam <- (colSums(spam) + 1)  / (n_spam * 100 + len)
x_normal <- (colSums(normal) + 1) / (n_normal * 100 + len)

##################
#Feature choosing#
##################

#choose features with larger difference in likelyhood probabilities
diff <- abs(x_normal - x_spam)
len <- 49
index <- order(diff,decreasing = T)[1:len]

spam <- data[data$is_spam == 1,index]
normal <- data[data$is_spam == 0,index]

n_spam <- dim(spam)[1]
n_normal <- dim(normal)[1]
n_sample <- dim(data)[1]

p_spam <- (n_spam + 1) / (n_sample + 2)
p_normal <- (n_normal + 1) / (n_sample + 2)

#use word_freq to calculate the p(word|y)
spam <- spam[,1:len]
normal <- normal[,1:len]

x_spam <- (colSums(spam) + 1)  / (n_spam * 100 + len)
x_normal <- (colSums(normal) + 1) / (n_normal * 100 + len)

########
##Test##
########

test <- read.csv("test.csv")
m <- dim(test)[1] #test sample numbers
n <- dim(test)[2] #test col numbers, to locate the lable col

test_data <- test[,index]

#calculate bayes probability separately for each test sample(row)
#and write the judgement into result[]

#the word_freq in test samples are used as exponent, 
#as in our assumption(email length = 100),
#the freq data given actually represents the times a word appears in an sample

result <- vector(length = m)
result[] <- 0

pr_spam <- vector(length = m)
pr_normal <- vector(length = m)

for (i in 1:m)
{
  p_data_spam <- prod(x_spam ^ test_data[i,])
  p_data_normal <- prod(x_normal ^ test_data[i,])
  
  #with divison of p(D)
  #which acctually can be ignored and using the numerator only is sufficient
  p_data <- p_spam * p_data_spam + p_normal * p_data_normal 
  pr_spam[i] <- p_spam * p_data_spam / p_data
  pr_normal[i] <- p_normal * p_data_normal / p_data
  if (pr_spam[i] > pr_normal[i]) 
  {
    result[i] <- 1
  }
}

##########
#Accuracy#
##########
real <- test[,n]
accu <- result == real
p_accu <- sum(accu) * 1.0 / m



  