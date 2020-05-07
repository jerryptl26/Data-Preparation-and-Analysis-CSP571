library(ggplot2)
library(caret)

#Importing dataset
URL = "https://archive.ics.uci.edu/ml/machine-learning-databases/statlog/german/german.data-numeric"
data  <-  read.csv(URL,sep='')

#Display the dataset
head(data)

#dimensions of dataset
nrow(data)
ncol(data)

colnames(data)[25]  <- c("response")
str(data)
typeof(data$response)
data$response=factor(data$response)
str(data)

#splitting data into train and test data
partition  <- createDataPartition(data$response,p=0.8,list=F)
train <- data[partition,]
test  <- data[-partition,]

#Fitting our logistic model
fitted_model  <- glm(response~.,data=train,family =binomial)

#predicting the results of glm on Train dataset
prob  <- predict(fitted_model,train,type="response")
pred_response  <- rep(1,nrow(train))
pred_response[prob > 0.5]  <- 2
pred_response<-factor(pred_response)
conf<-confusionMatrix(train$response,pred_response)
conf$byClass

#using trainControl and train functions
crossvalidation <- trainControl(method="cv",number = 10)
new_fitted_model <- train(response~.,data=train,method="glm",family=binomial,trControl=crossvalidation)
new_fitted_model

#predicting the results of crossvalidation on Train dataset
new_prob <- predict(new_fitted_model,train,type="prob")
new_pred_response  <- rep(1,nrow(train))
new_pred_response[new_prob[2] > 0.5]  <- 2
new_pred_response
new_pred_response<-factor(new_pred_response)
conf<-confusionMatrix(train$response,new_pred_response)
conf$byClass

#predicting the results of glm on Train dataset
prob  <- predict(fitted_model,test,type="response")
pred_response  <- rep(1,nrow(test))
pred_response[prob > 0.5]  <- 2
pred_response<-factor(pred_response)
conf<-confusionMatrix(test$response,pred_response)
conf$byClass

#predicting the results of crossvalidation on Test dataset
new_prob <- predict(new_fitted_model,test,type="prob")
new_pred_response  <- rep(1,nrow(test))
new_pred_response[new_prob[2] > 0.5]  <- 2
new_pred_response<-factor(new_pred_response)
conf<-confusionMatrix(test$response,new_pred_response)
conf$byClass

