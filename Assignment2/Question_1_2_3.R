
#Chapter 1
#Load Libraries:
library(MASS)
library(ggplot2)
library(gridExtra)

#Load dataset:
boston<-data.frame(Boston)
attach(Boston)

#Check number of rows:
nrow(boston)

#Check number of column:
ncol(boston)

#view top 6 rows of dataset:
head(boston)

#Check the datatypes of dataset:
str(boston)

#Display Summary:
summary(boston)

#fit a regression model:
res<-lm(medv~lstat,data=boston)
#Display Summary:
summary(res)

#plot Resulting fit:
plot(lstat,medv, col="lawngreen")
abline(res,col="red",lwd=2)

par(mfrow=c(2,2))
plot(res, col="lawngreen")

#plot of Fitted values Vs. Residuals
plot(res$fitted.values,res$residuals, col="lawngreen")
abline(res,col="red",lwd=2)

#plot for non-linear fit:
p1 <- ggplot(data=boston,aes(x=res$fitted,y=res$residuals)) +
  geom_point(col="lawngreen")+
  stat_smooth(col="red")+
  ggtitle("Non-Linear fit")
p1

coef(res)

#predictions for lstat values of 5, 10 and 15
test=data.frame(lstat=c(5,10,15))
predict(res,test,interval = "confidence")

predict(res,test,interval = "predict")


#modified regression model:
newres <- lm(medv~lstat+I(lstat^2),data=boston)
#summary for modified model:
summary(newres)
#comparison of R2 between linear and non-linear fit:

par(mfrow=c(2,2))
plot(newres,col="lawngreen")

#plot for fitted values vs residual:
p1 <- ggplot(data=boston,aes(x=res$fitted,y=res$residuals)) +
  geom_point(col="lawngreen")+
  stat_smooth(col="red")+
  ggtitle("Residual vs fitted values")
p1


#plot for non-linear fit:
p1 <- ggplot(data=boston,aes(x=lstat,y=medv)) +
  geom_point(col="lawngreen")+
  stat_smooth(formula = y ~ x + I(x^2),method="lm",col="red")+
  ggtitle("Non-Linear fit")
p1

anova(res,newres)
#========================================================================================================
#chapter2
#Load Libraries:
library(ggplot2)
library(lattice)
library(caret)
library(corrplot)
library(pROC)
## corrplot 0.84 loaded
#Load dataset:
URL<- "https://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data"
data <- read.table(URL,stringsAsFactors = FALSE, sep = ",") 
colnames(data) <- c("Sex","Length","Diameter","Height","Whole_Weigth","Shucked_weight","Viscera_weight","Shell_weight","Rings")

#Display Summary:
summary(data)

#Check number of rows:
nrow(data)

#Check number of column:
ncol(data)

#check for all available sex categories in dataset:
unique(data$Sex)

# Remove Infant category:
newdata <- data[ which(data$Sex!='I'), ]

#Check number of rows of new dataset:
nrow(newdata)

#Check number of columns of new dataset:
ncol(newdata)

#check for all sex categories in new dataset:
unique(newdata$Sex)

#Check for datatypes of new dataset:
str(newdata)
#transform the datatype of Sex attribute to factor type:
newdata$Sex<-factor(newdata$Sex)

#check for the change:
str(newdata)

#partition the dataset (80% for training and 20% for testing) 
partition <- createDataPartition(newdata$Sex, p = .8,list = FALSE)
Train <- newdata[ partition,]
Test  <- newdata[-partition,]

#fit the logistic regression model:
model  <- glm(Sex~.,data=Train,family=binomial)
model
#display summary of fitted model:
summary(model)
confint(model)

#predict the regression value for test data
t<-predict(model,Test,type= "response")

#use 50% cutoff to tag a male/female:
new_Temp=ifelse(t >0.5,"M","F")

#check the datatype of predicted result: 
str(new_Temp)

#convert the datatype to factor:
new_Temp<-factor(new_Temp)

#check for the change in datatype:
str(new_Temp)

#Display confusion matrix:
confusionMatrix(Test$Sex,new_Temp)

#ROC curve
roc(Test$Sex,t,plot = TRUE)

#Display coorelation between predictors:
M <- cor(newdata[-1])
corrplot(M, method = "color", tl.col = "black", tl.srt = 90)

#=========================================================================================================================
#chapter 3

#load libraries:
library(caTools)
library(e1071)

#Load dataset:
URL <- "https://archive.ics.uci.edu/ml/machine-learning-databases/mushroom/agaricus-lepiota.data"
m_dataset <- read.table(URL,stringsAsFactors = FALSE, sep = ",") 
colnames(m_dataset) <- c("class","cap-shape","cap-surface","cap-color","bruises","odor","gill-attachment","gill-spacing","gill-size","gill-color","stalk-shape","stalk-root","stalk-surface-above-ring","stalk-surface-below-ring","stalk-color-above-ring","stalk-color-below-ring","veil-type","veil-color","ring-number","ring-type","spore-print-color","population","habitat")

#Display Summary:
summary(m_dataset)
View(m_dataset)
#Check the datatypes of dataset:
str(m_dataset)


#check for different categories for attribute stalk-root
unique(m_dataset$`stalk-root`)

#check for total number of columns and rows in a dataset:
dim(m_dataset)

#calculate the total missing values:
cnt <- sum(m_dataset$`stalk-root`=='?')
cnt

#remove all the rows containing missing values: 
m_newdataset <- m_dataset[ which(m_dataset$'stalk-root'!='?'), ]

#calculate number of rows for each target class(poisonous and edible)
table(m_newdataset$class)

#display total number of rows after removing missing values:
nrow(m_newdataset)

#display the datatype of attributes: 
str(m_newdataset)

#convert the datatype of response attribute to factor:
m_newdataset$class=factor(m_newdataset$class)

#split the dataset(80% for training and 20% for testing) 
set.seed(123)  
sample = sample.split(m_newdataset,SplitRatio = 0.80)
train1 =subset(m_newdataset,sample ==TRUE)
test1=subset(m_newdataset, sample==FALSE)

#fitting the model using Naive Bayes classifier:
nb_model<-naiveBayes(m_newdataset$class~.,data=train1)
nb_model<-naiveBayes(train1[-1],train1$class)

#make prediction on testing data:
nb_test_predict<-predict(nb_model,test1[,-1])

#make prediction on training data:
nb_train_predict<-predict(nb_model,train1[,-1])

#Accuracy calculation for training data:
mean(nb_train_predict==train1$class)

#Accuracy calculation for testing data: 
mean(nb_test_predict==test1$class)

#confusion matrix
table(nb_test_predict,test1$class)

