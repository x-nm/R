###########################################################
#Bayes spam classifier 
#created 2016.03.28
#modified 2016.04.01
#changed modeling by word 0-1 to modeling by word frequency
#modified 2016.04.02
#added the last 3 features
#by Tan Linuo
###########################################################

data <- read.csv("train.csv")
data[,"capital_run_length_average"] <- 100 * data[,"capital_run_length_average"] / data[,"capital_run_length_total"]
data[,"capital_run_length_longest"] <- 100 * data[,"capital_run_length_longest"] / data[,"capital_run_length_total"]

len <- dim(data)[2] - 2 
spam <- data[data$is_spam == 1,1:len]
normal <- data[data$is_spam == 0,1:len]

#len <- dim(data)[2] - 4 #use only word_freq and char_freq as features
#spam <- spam[,1:len] 
#normal <- normal[,1:len]

n_spam <- dim(spam)[1]
n_normal <- dim(normal)[1]
n_sample <- dim(data)[1]

p_spam <- (n_spam + 1) / (n_sample + 2)
p_normal <- (n_normal + 1) / (n_sample + 2)

#use word_freq to calculate the p(word|y)
#based on the data given, assume the length of each email to be 100,
#and the data given (word_freq(*100 already)) will then represent 
#count of the word in an email sample

x_spam <- (colSums(spam) + 1)  / (n_spam * 100 + len)
x_normal <- (colSums(normal) + 1) / (n_normal * 100 + len)

########
##Test##
########

test <- read.csv("test.csv")
test[,"capital_run_length_average"] <- 100 * test[,"capital_run_length_average"] / test[,"capital_run_length_total"]
test[,"capital_run_length_longest"] <- 100 * test[,"capital_run_length_longest"] / test[,"capital_run_length_total"]

m <- dim(test)[1] #test sample numbers
n <- dim(test)[2] #test col numbers

test_data <- test[,1:len]

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


