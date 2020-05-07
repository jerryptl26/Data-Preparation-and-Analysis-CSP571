library(ggplot2)
library(lattice)
library(caret)
#Importing the dataset
data <- data.frame(swiss)

#display the dataset
head(data)

#dimensions of dataset
nrow(data)
ncol(data)

#80-20 split using createDataPartition
set.seed(150)
partition <- createDataPartition(data$Fertility,p=0.8,list = F)
train <- data[partition,]
test <- data[-partition,]

#fitting a linear fit
model <- lm(Fertility~.,train)

summary(model)

#calculating test mse
mean((test$Fertility-predict(model,test))^2)

#lasso
# Loaging the library
library(Matrix)
library(foreach)
library(glmnet)
# Getting the independent variable
x <- model.matrix(Fertility~.,train)[,-1]

# Getting the dependent variable
y <- train$Fertility

# Setting the range of lambda values
lambda_seq <- 10^seq(5,-5,by = -.1)

# Using cross validation glmnet
lasso_cv <- cv.glmnet(x, y, alpha = 1,lambda = lambda_seq)
plot(lasso_cv)

#Best lambda value
best_lambda <- lasso_cv$lambda.min
best_lambda

# Using glmnet function to build the ridge regression model
fit <- glmnet(x, y, alpha = 1, lambda  = best_lambda)

# Checking the model
summary(fit)

#for testdata
xx =  model.matrix(Fertility~.,test)[,-1]
model_predict <- predict(fit,s =,newx = xx, type = "response")

#MSE on test data
mean((model_predict-test$Fertility)^2)

#coefficients
coef(model)
coef(lasso_cv)

