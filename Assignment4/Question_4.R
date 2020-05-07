#Import dataset
URL <- "https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv"
data <- read.csv(URL,sep=";")

#display dataset
head(data)

#exclude quality variable
n_data <- data[,-12]

#check the mean 
apply(n_data,2,mean)

#check the variance 
apply(n_data,2,var)

#apply scaling
n_n_data <- scale(n_data,center = TRUE,scale=TRUE)

#hierarchical clustering using complete linkage
complete <- hclust(dist(n_n_data),method="complete")
#plot for complete linkage
plot(complete)

#hierarchical clustering using single linkage
single <- hclust(dist(n_n_data),method="single")
#plot for single linkage
plot(single)

#for complete linkage
tail(complete$height,1)

#for single linkage
tail(single$height,1)

#applying cutree method on complete linkage
cut_complete <- cutree(complete,h=tail(complete$height,1))

#Number of clusters formed
table(cut_complete)
plot(complete)
rect.hclust(complete ,h=27.73476, border = 2:6)
abline(h =tail(complete$height,1), col = 'red')


library(dplyr)

complete$clusters <- cut_complete
complete <- dplyr::group_by(complete,Clusters)
a <- dplyr::summarise_each(wine2, funs(mean))
print.data.frame(a)