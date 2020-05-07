#Chapter 1
#Load Libraries
library(gridExtra)
library(ggplot2)


#load iris dataset
iris <-data.frame(iris)

#boxplot for attributes:
boxplot(iris,main="Box Plot for different features",xlab="Features",ylab="length in cm", col = "salmon")

#Inter Quartile range calculation:
IQR(iris$Sepal.Length)
IQR(iris$Sepal.Width)
IQR(iris$Petal.Length)
IQR(iris$Petal.Width)

#standart-deviation calculation
sd(iris$Sepal.Length)
sd(iris$Sepal.Width)
sd(iris$Petal.Length)
sd(iris$Petal.Length)

#Boxplot of attributes with class species 
library(ggplot2)
a <- ggplot(iris, aes(Species, Sepal.Length,  fill=Species)) + geom_boxplot()
b <-ggplot(iris, aes(Species, Sepal.Width,  fill=Species)) +  geom_boxplot()
c <-ggplot(iris, aes(Species, Petal.Length,  fill=Species))  +   geom_boxplot()
d <-ggplot(iris, aes(Species, Petal.Width,  fill=Species))  +   geom_boxplot()

grid.arrange(a + ggtitle(""),
             b + ggtitle(""),
             c + ggtitle(""),
             d + ggtitle(""),
             nrow = 2)

#chapter 2
#load library
library(moments)

#load tree sample dataset
trees <-data.frame(trees)

#summary of attributes
summary(trees)

#Histogram of attributes
par(mfrow=c(3,1))
hist(trees$Girth,main="Histogram for Girth", xlab = "Girth", ylab = "Count",border="black", col="salmon",las=1)
hist(trees$Height,main="Histogram for Height", xlab = "Height", ylab = "Count",border="black", col="salmon",las=1)
hist(trees$Volume,main="Histogram for volume", xlab = "Volume", ylab = "Count",border="black", col="salmon",las=1)

#Import library skewness
library(moments)

#Skewness Calculation
skewness(trees$Girth, na.rm = FALSE)
skewness(trees$Height, na.rm = FALSE)
skewness(trees$Volume, na.rm = FALSE)

#Chapter 3
#Load the auto-mpg sample dataset from the UCI Machine Learning Repository
URL <-"https://archive.ics.uci.edu/ml/machine-learning-databases/auto-mpg/auto-mpg.data"
ndata <- read.table(URL,stringsAsFactors = FALSE)
colnames(ndata) <- c("mpg","cylinders","displacement","horsepower","weight","acceleration","model_year","origin","car_name")

#Display number of rows
nrow(ndata)

#Display number of rows:
ncol(ndata)

#Diplay first 6 rows:
head(ndata)

#Display structure of variables:
str(ndata)    

#Converting datatype of column horsepower from character to numeric:
ndata$horsepower<- as.numeric(ndata$horsepower)

#Checking datatype of horsepower column whether it is converted to numeric value or not:
is.numeric(ndata$horsepower)

#Display structure of variables
str(ndata)

#Calculate median of column horsepower:
median_horsepower<-median(ndata$horsepower,na.rm = TRUE)
median_horsepower

#Calculate mean of column horsepower:
mean_horsepower<-mean(ndata$horsepower,na.rm = TRUE)
mean_horsepower

#Calculate number of rows having null value for column horsepower:
num<-sum(is.na(ndata$horsepower))
num

#Replace NA values of horsepower column with its median value:
ndata$horsepower[is.na(ndata$horsepower)]<-median_horsepower

#Check number of rows having null value for column horsepower:
num<-sum(is.na(ndata$horsepower))
num

#Check the new mean
mean(ndata$horsepower)


