# Creating a neural network model

# Fetching the libraries
require(xlsx)
require(caret)
require(nnet)
require(neuralnet)

# Reading the dataset

setwd("Required")
mydata <- read.xlsx("required.xlsx", sheetIndex = 2)

# Viewing the original dataset

str(mydata)

# Completing the pre-requisites

mydata$Customer <- as.factor(mydata$Customer)
mydata$Despatch.Area <- as.factor(mydata$Despatch.Area)
mydata$Transporter.Code <- as.factor(mydata$Transporter.Code)
mydata$Customer <- NULL
mydata$Vehicle.Num <- NULL

# Dividing into training and test dataset and o/p is net.value

sample <- createDataPartition(mydata$Net.Retention.Amount, p = 0.7, list = FALSE)
train <- mydata[sample,]
test <- mydata[-sample,]

str(train)
str(test)

# Looking at the training data

# Preprocessing the training data

num_train <- train[,-c(1:5,14)]
fac_train <- train[,c(1:5,14)]

processed <- preProcess(num_train, method = "range")
ranges <- processed$ranges
new_num_train <- predict(processed, num_train)

# Dummify the training data

dummydata_train <- dummyVars(" ~.", data = fac_train)
dummydata_train_df <- data.frame(predict(dummydata_train, newdata = fac_train))

# New training data

train_new <- data.frame(new_num_train, dummydata_train_df)

# Looking at the testing data

# Preprocessing the testing data

num_test <- test[,-c(1:5,14)]
fac_test <- test[,c(1:5,14)]

processed_test <- preProcess(num_test, method = "range")
ranges_test <- processed_test$ranges
new_num_test <- predict(processed_test, num_test)

# Dummify the testing data

dummydata_test <- dummyVars(" ~.", data = fac_test)
dummydata_test_df <- data.frame(predict(dummydata_test, newdata = fac_test))

# New testing data

test_new <- data.frame(new_num_test, dummydata_test_df)

# Making the model

n <- names(train_new)

f <- as.formula(paste("Net.Retention.Amount ~", paste(n[!n %in% "Net.Retention.Amount"], collapse = " + ")))
nn <- neuralnet(f, data=train_new,hidden=10,stepmax=1e6)

# Predicting the output

pred.value <- compute(nn,test_new[,-which(names(test_new)=="Net.Retention.Amount")])
predicted_scaled <- pred.value$net.result

# Function for reversing the range function

reverse_range.value <- function(value, ranges){
  return((ranges[2,8]-ranges[1,8])*value 
         + ranges[1,8])
}

# Applying the function
predicted_unscaled.value <- sapply(1, function(x) reverse_range.value(predicted_scaled[,x],
                                                                              ranges))
d <- data.frame(round(predicted_unscaled.value,1), test$Net.Retention.Amount)

write.csv(d, file = "plotting.csv")
