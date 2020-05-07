library("factoextra")
library(tidyverse)
#load the dataset
data("USArrests")

#convert the dataset to a dataframe 
data <- data.frame(USArrests)

#checking the mean of the predictors
apply(data,2,mean)

#checking the variance of the predictors
apply(data,2,var)

#scaling the dataset
n_data <- scale(data, center = TRUE, scale = TRUE)

#Applying K-Means
result <- function(k)
{
  kmeans(n_data,centers=k,nstart=20)$tot.withinss
}
# values of k form 2 to 10
k <- 2:10

#compute total within-cluster sum of square values of k from 2 to 10
wss_val <- map_dbl(k, result)
wss_val

#Using elbow method to find optimal K value
plot(k, wss_val,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

#another Method
fviz_nbclust(n_data, kmeans, method = "wss")

#plots
optimal <- kmeans(n_data, centers = 4, nstart = 20)
fviz_cluster(optimal, data = n_data)
