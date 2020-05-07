library(ggplot2)
library(caret)
library(readr)
library(data.table)
#Importing the dataset
URL ="http://archive.ics.uci.edu/ml/machine-learning-databases/00243/yacht_hydrodynamics.data"
data = fread(URL, header = FALSE)
colnames(data)  <- c("Longitudinal_position","Prismatic_coefficient","Length-displacement","Beam_drought","Length_beam","Froude_no","Residuary")

#display the dataset
head(data)

#dimensions of dataset
nrow(data)
ncol(data)

#splitting data into train and test data
partition  <- createDataPartition(data$Longitudinal_position,p=0.8,list=F)
train <- data[partition,]
test  <- data[-partition,]

#Fitting a linear model
fitted_model  <- lm(Residuary~.,data= train)

#Summary of linear model
summary(fitted_model)

#Calculating Training MSE
mean((train$Residuary-predict(fitted_model,train))^2,na.rm=T)

#Calculating Training RMSE
sqrt(mean((train$Residuary-predict(fitted_model,train))^2,na.rm=T))

#Finding R2 value 
rss  <-  sum((train$Residuary-predict(fitted_model,train))^2,na.rm=T) 
tss  <- sum((train$Residuary-mean(train$Residuary,na.rm=T))^2,na.rm=T)
r  <- 1- (rss/tss) 
r

#using trainControl method to perform bootstrap
bootstrap  <- trainControl(method="boot",number = 1000)
new_fitted_model <- train(Residuary~.,data=train,method="lm",trControl=bootstrap,na.action = na.pass)

#checking the fit
new_fitted_model

#Histogram
hist(new_fitted_model$resample$RMSE,main="Histogram of RMSE",xlab="RMSE Values",col="beige")

#Calculating Training MSE
(mean(new_fitted_model[["resample"]][["RMSE"]]))^2

#Calculating Training RMSE
mean(new_fitted_model[["resample"]][["RMSE"]])

#Calculating R2
mean(new_fitted_model[["resample"]][["Rsquared"]])
#-------------------------------------
#Calculating Training MSE
mean((train$Residuary-predict(new_fitted_model,train))^2,na.rm=T)

#Calculating Training RMSE
sqrt(mean((train$Residuary-predict(new_fitted_model,train))^2,na.rm=T))

#Finding R2 value 
rss  <-  sum((train$Residuary-predict(new_fitted_model,train))^2,na.rm=T) 
tss  <- sum((train$Residuary-mean(train$Residuary,na.rm=T))^2,na.rm=T)
r <- 1- (rss/tss) 
r
#---------------------------------------
#Results of linear model on Test data
#Calculating Testing MSE 
mean((test$Residuary-predict(fitted_model,test))^2,na.rm=T)

#Calculating Testing RMSE 
sqrt(mean((test$Residuary-predict(fitted_model,test))^2,na.rm=T))

#calculating TEST R2 
rss  <-  sum((test$Residuary-predict(fitted_model,test))^2,na.rm=T) 
tss  <- sum((test$Residuary-mean(test$Residuary,na.rm=T))^2,na.rm=T) 
r  <- 1- (rss/tss) 
r

#Results of bootstrap model on Test data
#Calculating Testing MSE
mean((test$Residuary-predict(new_fitted_model,test))^2,na.rm=T)

#Calculating Testing RMSE 
sqrt(mean((test$Residuary-predict(new_fitted_model,test))^2,na.rm=T))

#calculating TEST R2 
rss  <-  sum((test$Residuary-predict(new_fitted_model,test))^2,na.rm=T) 
tss  <- sum((test$Residuary-mean(test$Residuary,na.rm=T))^2,na.rm=T) 
r  <- 1- (rss/tss) 
r

