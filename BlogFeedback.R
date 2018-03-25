library(caret)

MyData <- read.csv(file="/home/akriti/Documents/MachineLEarning/BlogFeedbackAssignment1/blogData_train.csv", header=TRUE, sep=",")

basic_features <- MyData[,c(51:60,281)]
textual_feature <- MyData[,c(63:262,281)]

#Consider 10 percent of the data
indexes = sample(1:nrow(basic_features), size=0.1*nrow(basic_features))
basic_features = basic_features[indexes,]
indexes = sample(1:nrow(textual_feature), size=0.1*nrow(textual_feature))
textual_feature = textual_feature[indexes,]


#Rename the column names to maintain consistency of the column names in Train data and Test data
renameBasicFeaturesVal <- function(x){
   x <-paste("A", 51:61, sep="")
}

renameTextualFeaturesVal <- function(x){
  x <-paste("A", 63:263, sep="")
}


names(basic_features) <- renameBasicFeaturesVal(names(basic_features))
names(textual_feature) <- renameTextualFeaturesVal(names(textual_feature))

#Read Test data
MyTest1 <- read.csv(file="/home/akriti/Documents/MachineLEarning/BlogFeedbackAssignment1/blogData_test-2012.02.28.00_00.csv", header=TRUE, sep=",")
MyTest2 <- read.csv(file="/home/akriti/Documents/MachineLEarning/BlogFeedbackAssignment1/blogData_test-2012.02.29.00_00.csv", header=TRUE, sep=",")
MyTest3 <- read.csv(file="/home/akriti/Documents/MachineLEarning/BlogFeedbackAssignment1/blogData_test-2012.03.30.01_00.csv", header=TRUE, sep=",")
MyTest4 <- read.csv(file="/home/akriti/Documents/MachineLEarning/BlogFeedbackAssignment1/blogData_test-2012.03.31.01_00.csv", header=TRUE, sep=",")

#filter the columns according to the problem statement
basic_features_test1 <- MyTest1[,c(51:60,281)]
basic_features_test2 <- MyTest2[,c(51:60,281)]
basic_features_test3 <- MyTest3[,c(51:60,281)]
basic_features_test4 <- MyTest4[,c(51:60,281)]

#Rename column names to keep the Train data and Test data column names in sync
names(basic_features_test1) <- renameBasicFeaturesVal(names(basic_features_test1))
names(basic_features_test2) <- renameBasicFeaturesVal(names(basic_features_test2))
names(basic_features_test3) <- renameBasicFeaturesVal(names(basic_features_test3))
names(basic_features_test4) <- renameBasicFeaturesVal(names(basic_features_test4))


textual_features_test1 <- MyTest1[,c(63:262, 281)]
textual_features_test2 <- MyTest2[,c(63:262,281)]
textual_features_test3 <- MyTest3[,c(63:262,281)]
textual_features_test4 <- MyTest4[,c(63:262,281)]
names(textual_features_test1) <- renameTextualFeaturesVal(names(textual_features_test1))
names(textual_features_test2) <- renameTextualFeaturesVal(names(textual_features_test2))
names(textual_features_test3) <- renameTextualFeaturesVal(names(textual_features_test3))
names(textual_features_test4) <- renameTextualFeaturesVal(names(textual_features_test4))



####################################
#Experiment 1
####################################
################################
#Linear regression
#Training a model - means you are actually creating a y=a1x1+a2x2+..+c equation
fit.basic.features.train <- lm(formula = A61 ~ A51+A52+A53+A54+A55+A56+A57+A58+A59+A60, data = basic_features)
fit.basic.features.train
summary(fit.basic.features.train)

#Predict values for Test data 1
#Calculating the values of y with the coefficients calculated in the training model and the values of the variables from the test data
#Comparing the predicted y values with the actual y values in the test data
pred_basic_features_1 <-predict(fit.basic.features.train,basic_features_test1,se.fit=TRUE)
MSE = mean((basic_features_test1$A61 - pred_basic_features_1$fit)^2)
print(cat("MSE For Experiment 1: ",MSE))
#More the variation, worse the model

#Predict values for Test data 2
pred_basic_features_2 <-predict(fit.basic.features.train,basic_features_test2,se.fit=TRUE)
MSE = mean((basic_features_test2$A61 - pred_basic_features_2$fit)^2)
print(cat("MSE For Experiment 2: ",MSE))


#Predict values for Test data 3
pred_basic_features_3 <-predict(fit.basic.features.train,basic_features_test3,se.fit=TRUE)
MSE = mean((basic_features_test3$A61 - pred_basic_features_3$fit)^2)
print(cat("MSE For Experiment 3: ",MSE))


#Predict values for Test data 4
pred_basic_features_4 <-predict(fit.basic.features.train,basic_features_test4,se.fit=TRUE)
MSE = mean((basic_features_test4$A61 - pred_basic_features_4$fit)^2)
print(cat("MSE For Experiment 4: ",MSE))

#Predict values for the Training model
pred_basic_features <-predict(fit.basic.features.train,basic_features,se.fit=TRUE)
MSE = mean((basic_features$A61 - pred_basic_features$fit)^2)
print(cat("MSE For Basic Features train model: ",MSE))
################################
#Logistic Regression
#Calculate mean to use it for changing the target variable to Binary (Remove all NA values)
basic_features_mean <- mean(basic_features$A61, NA.rm = TRUE)
#basic_features_log <- MyData[,c(51:60,function(x){x => basic_features_mean ? 1 : 0})]
#ncol(basic_features) 
#for(i in 1:nrow(basic_features)){
#  basic_features_log <- basic_features[,-11]
#  if(basic_features[,11] < basic_features_mean){
#    basic_features_log$A61 <- 0
#  }else{
#    basic_features_log$A61 <- 1
#  }
#}
#basic_features_log

#Alternative way to add 0 or 1 on the basis of the mean
basic_features_log_n <- basic_features
basic_features_log_n$A61 <- sapply(basic_features_log_n$A61, function(x) {ifelse(x >= basic_features_mean, 1, 0)})
logfit.basic.features.train <- glm(formula = A61 ~ A51+A52+A53+A54+A55+A56+A57+A58+A59+A60, data = basic_features_log_n, family=binomial())
summary(logfit.basic.features.train)

#Change binary values for test data target variables
#Test data 1
basic_features_test1_mean <- mean(basic_features_test1$A61, NA.rm = TRUE)
basic_features_test1_n <- basic_features_test1
basic_features_test1_n$A61 <- sapply(basic_features_test1_n$A61, function(x) {ifelse(x >= basic_features_test1_mean, 1, 0)})
log_pred_basic_features_1 <-predict.glm(logfit.basic.features.train,basic_features_test1_n,se.fit=TRUE)
predicted_log <- sapply(log_pred_basic_features_1$fit, function(x){ifelse(x > .5, 1, 0)})
typeof(basic_features_test1_n$A61)
print(confusionMatrix(predicted_log, basic_features_test1_n$A61))

#Test data 2
basic_features_test2_mean <- mean(basic_features_test2$A61, NA.rm = TRUE)
basic_features_test2_n <- basic_features_test1
basic_features_test2_n$A61 <- sapply(basic_features_test2_n$A61, function(x) {ifelse(x >= basic_features_test2_mean, 1, 0)})
log_pred_basic_features_2 <-predict.glm(logfit.basic.features.train,basic_features_test2_n,se.fit=TRUE)
predicted_log <- sapply(log_pred_basic_features_2$fit, function(x){ifelse(x > .5, 1, 0)})
print(confusionMatrix(predicted_log, basic_features_test2_n$A61))

####################################
#Experiment 2
####################################
textual_feature
################################
#Linear regression
#Training a model - means you are actually creating a y=a1x1+a2x2+..+c equation
fit.textual.features.train <- lm(textual_feature$A263 ~ ., data = textual_feature)
fit.textual.features.train
summary(fit.textual.features.train)

#Test data 1
pred_textual_features_1 <-predict(fit.textual.features.train,textual_features_test1,se.fit=TRUE)
MSE = mean((textual_features_test1$A263 - pred_textual_features_1$fit)^2)
print(cat("MSE For Experiment 2: ",MSE))


#Test data 2
pred_textual_features_2 <-predict(fit.textual.features.train,textual_features_test2,se.fit=TRUE)
MSE = mean((textual_features_test2$A263 - pred_textual_features_2$fit)^2)
print(cat("MSE For Experiment 2: ",MSE))



#Test data 3
pred_textual_features_3 <-predict(fit.textual.features.train,textual_features_test3,se.fit=TRUE)
MSE = mean((textual_features_test3$A263 - pred_textual_features_3$fit)^2)
print(cat("MSE For Experiment 2: ",MSE))


#Test data 4
pred_textual_features_4 <-predict(fit.textual.features.train,textual_features_test4,se.fit=TRUE)
MSE = mean((textual_features_test4$A263 - pred_textual_features_4$fit)^2)
print(cat("MSE For Experiment 2: ",MSE))


#Train data
pred_textual_features <-predict(fit.textual.features.train,textual_features,se.fit=TRUE)
MSE = mean((textual_features$A263 - pred_textual_features$fit)^2)
print(cat("MSE For Train data: ",MSE))
################################
#Logistic Regression

#Calculate mean to use it for changing the target variable to Binary (Remove all NA values)
textual_feature_mean <- mean(textual_feature$A263, NA.rm = TRUE)

textual_feature_log_n <- textual_feature
textual_feature_log_n$A263 <- sapply(textual_feature_log_n$A263, function(x) {ifelse(x >= textual_feature_mean, 1, 0)})
logfit.textual.features.train <- glm(formula = textual_feature_log_n$A263 ~ ., data = textual_feature_log_n, family=binomial())

summary(logfit.basic.features.train)

#Test data 1
textual_features_test1_mean <- mean(textual_features_test1$A263, NA.rm = TRUE)
textual_features_test1_n <- textual_features_test1
textual_features_test1_n$A263 <- sapply(textual_features_test1_n$A263, function(x) {ifelse(x >= textual_features_test1_mean, 1, 0)})
log_pred_textual_features_1 <-predict.glm(logfit.textual.features.train,textual_features_test1_n,se.fit=TRUE)
predicted_textual_log <- sapply(log_pred_textual_features_1$fit, function(x){ifelse(x > .5, 1, 0)})
typeof(textual_features_test1_n$A263)
typeof(predicted_textual_log)
print(confusionMatrix(predicted_textual_log, textual_features_test1_n$A263))



#Test data 2
textual_features_test2_mean <- mean(textual_features_test2$A263, NA.rm = TRUE)
textual_features_test2_n <- textual_features_test2
textual_features_test2_n$A263 <- sapply(textual_features_test2_n$A263, function(x) {ifelse(x >= textual_features_test2_mean, 1, 0)})
log_pred_textual_features_2 <-predict.glm(logfit.textual.features.train,textual_features_test2_n,se.fit=TRUE)
predicted_textual_log <- sapply(log_pred_textual_features_2$fit, function(x){ifelse(x > .5, 1, 0)})
print(confusionMatrix(predicted_textual_log, textual_features_test2_n$A263))



#Test data 3
textual_features_test3_mean <- mean(textual_features_test3$A263, NA.rm = TRUE)
textual_features_test3_n <- textual_features_test3
textual_features_test3_n$A263 <- sapply(textual_features_test3_n$A263, function(x) {ifelse(x >= textual_features_test3_mean, 1, 0)})
log_pred_textual_features_3 <-predict.glm(logfit.textual.features.train,textual_features_test3_n,se.fit=TRUE)
predicted_textual_log <- sapply(log_pred_textual_features_3$fit, function(x){ifelse(x > .5, 1, 0)})
print(confusionMatrix(predicted_textual_log, textual_features_test3_n$A263))


#Test data 4
textual_features_test4_mean <- mean(textual_features_test4$A263, NA.rm = TRUE)
textual_features_test4_n <- textual_features_test4
textual_features_test4_n$A263 <- sapply(textual_features_test4_n$A263, function(x) {ifelse(x >= textual_features_test4_mean, 1, 0)})
log_pred_textual_features_4 <-predict.glm(logfit.textual.features.train,textual_features_test4_n,se.fit=TRUE)
predicted_textual_log <- sapply(log_pred_textual_features_4$fit, function(x){ifelse(x > .5, 1, 0)})
print(confusionMatrix(predicted_textual_log, textual_features_test4_n$A263))

