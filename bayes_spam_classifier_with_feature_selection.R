################################################
#Bayes spam classifier WITH feature selection 
#feature selection is done by substract features
#2016.04.02
#by Tan Linuo
################################################


# function name: trainFunc
# input:
#   -data: train data
#   -feature: feature index, such as 1:len, or a index array
#   -test: testfile
# output:
# accuracy of the training model
trainFunc <- function(data, test_data, feature){
  ##############################################
  #train                                       #
  ##############################################
  spam <- data[data$is_spam == 1,feature]
  normal <- data[data$is_spam == 0,feature]
  
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
  
  ##############################################
  #test                                        #
  ##############################################
  m <- dim(test_data)[1] #test sample numbers
  n <- dim(test_data)[2] #test col numbers, to locate the col index of lable
  real <- test_data[,n]
  test_data <- test_data[,feature]
  
  #calculate bayes probability separately for each test sample(row)
  #and write the judgement into result[]
  
  #the word_freq in test samples are used as exponent, 
  #as in our assumption(email length = 100),
  #the freq data given actually represents the times a word appears in an sample
  result <- vector(length = m)
  result[] <- 0
  
  for (i in 1:m)
  {
    p_data_spam <- prod(x_spam ^ test_data[i,])
    p_data_normal <- prod(x_normal ^ test_data[i,])
    
    #with divison of p(D)
    #which acctually can be ignored and using the numerator only is sufficient
    p_data <- p_spam * p_data_spam + p_normal * p_data_normal 
    pr_spam <- p_spam * p_data_spam / p_data
    pr_normal <- p_normal * p_data_normal / p_data
    if (pr_spam > pr_normal) 
    {
      result[i] <- 1
    }
  }
  
  #Accuracy
  accu <- result == real
  sum(accu) * 1.0 / m
}

# import data and do initialization
train <- read.csv("train.csv")
test <- read.csv("test.csv")

len_ori <- dim(train)[2] - 4 # only freq features were chosen to train this model
len <- len_ori
index <- 1:len # initialize the index as all features available

# results before feature selection
p_accu_all <- trainFunc(train, test, index) # accuracy initialization

# feature selection
p_accu <- 0
count <- 0
while (count + len <= len_ori)
{
  for (i in 1:len)
  {
    # delete one feature at a time
    # if the accuracy remains or increases, then delete this feature for good
    p_accu <- trainFunc(train, test, index[-i])
    if (p_accu >= p_accu_all)
    {
      index <- index[-i]
      len <- length(index)
      p_accu_all <- p_accu
      break
    }
  }
  count <- count + 1
}

accuracy_final <- trainFunc(train, test, index)
