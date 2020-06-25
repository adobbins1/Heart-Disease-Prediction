library(ggplot2)
library(ggplot2)
library(purrr)
library(tidyr)
library(dplyr)
library(MASS)
library(randomForest)
library(caret)
library(e1071)
library(glmnet)
library(plotly)
library(aod)

heart <- transform(
  heart,
  age=as.integer(age),
  sex=as.factor(sex),
  cp=as.factor(cp),
  trestbps=as.integer(restbps),
  choi=as.integer(chol),
  fbs=as.factor(fbs),
  restecg=as.factor(restecg),
  thalach=as.integer(thalach),
  exang=as.factor(exang),
  oldpeak=as.numeric(oldpeak),
  slope=as.factor(slope),
  ca=as.factor(ca),
  thai=as.factor(thal),
  num=as.factor(status)
)

sapply(heart, class)

par(mfrow = c(2,7))
hist(heart$age)
hist(heart$sex)
hist(heart$cp)
hist(heart$restbps)
hist(heart$chol)
hist(heart$fbs)
hist(heart$restecg)
hist(heart$thalach)
hist(heart$exang)
hist(heart$oldpeak)
hist(heart$slope)
hist(heart$ca)
hist(heart$thal)
hist(heart$status)


# Random Forest

# Split Data Into Testing and Validation Sets 
set.seed(100)
train <- sample(nrow(heart), 0.7*nrow(heart), replace = FALSE)
trainset <- heart[train,]
validset <- heart[-train,]
summary(trainset)
summary(validset)

# Check the Class of each Attribute
sapply(trainset, class)

# Create the Model
model1 <- randomForest(formula = num ~ ., data = trainset, importance = TRUE)
model1

# Predicting the Training Set
predtrain <- predict(model1, trainset, type = "class")
# Checking the Classification Accuracy of Training Set
table(predtrain, trainset$num)

# Predicing the Validation Set
predvalid <- predict(model1, validset, type = "class")
# Checking the Classification Accuracy of Validation Set
mean(predvalid == validset$num)
table(predvalid, validset$num) 

# Accuracy
confusionMatrix(predvalid, validset$num)

#SVM 

# Creating SVM Model
svmModel <- svm(formula = num ~ ., data = trainset, type = 'C-classification', 
                kernel = 'linear' )
svmModel
# Predicting Training Set
predtrain2 <- predict(svmModel, trainset, type = 'class')

# Checking the Classification Accuracy of Training Set
table(predtrain2, trainset$num)

# Predicing the Validation Set
predvalid2 <- predict(svmModel, validset, type = 'class')

# Checking the Classification Accuracy of Validation Set
mean(predvalid2 == validset$num)
table(predvalid2, validset$num)

# Accuracy
confusionMatrix(predvalid2, validset$num)

# Naive Bayes

# Creating Naive Bayes Model 
nbModel <- naiveBayes(formula = num ~ ., data = trainset)
nbModel

# Predicting Training Set
predtrain3 <- predict(nbModel, trainset, type = 'class')
table(predtrain3, trainset$num)

# Predicting Validation Set 
predvalid3 <- predict(nbModel, validset, type = 'class')
table(predvalid3, validset$num)

# Accuracy
confusionMatrix(predvalid3, validset$num)


