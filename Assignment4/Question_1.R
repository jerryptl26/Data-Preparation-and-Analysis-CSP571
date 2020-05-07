library(rpart)
library(rpart.plot)

#function Definition
gini <- function(p) 
{
  gini.index = 2 * p * (1 - p)
  return (gini.index)
}

entropy <- function(p) 
{
  entropy = (p * log(p) + (1 - p) * log(1 - p))
  return (entropy)
}

set.seed(150)
a<-rnorm(n=150,mean=5,sd=2)
b<-rnorm(n=150,mean=-5,sd=2)
data1 <- data.frame(val = a,label=rep("y",150))
data2 <- data.frame(val = b,label=rep("n",150))
data <- rbind(data1,data2)
data$label <- as.factor(data$label)
d_tree <- rpart(label~val,data,method="class")
rpart.plot(d_tree)

#Calculating Gini and Entropy for Each Node:
#p=probability of each node
p=c(.5, 0, 1)

gini_values=sapply(p, gini)
gini_values
entropy_values=sapply(p, entropy)
entropy_values

set.seed(150)
a<-rnorm(n=150,mean=1,sd=2)
b<-rnorm(n=150,mean=-1,sd=2)
data1 <- data.frame(val = a,label=rep("y",150))
data2 <- data.frame(val = b,label=rep("n",150))
data <- rbind(data1,data2)
data$label <- as.factor(data$label)
d_tree <- rpart(label~val,data,method="class")
rpart.plot(d_tree)

#Calculating Gini and Entropy for Each Node:
#p=probability of each node
p=c(.5,0.22,0.72,0.28,0.53,0.45,0.09,0.23,0.70,0.37,0.59,1.0,0.81)
gini_values=sapply(p, gini)
gini_values
entropy_values=sapply(p, entropy)
entropy_values

new_d_tree <- prune.rpart(d_tree,cp=0.1)
rpart.plot(new_d_tree)

#Calculating Gini and Entropy for Each Node:
#p=probability of each node
p=c(.5,0.22,0.72)
gini_values=sapply(p, gini)
gini_values
entropy_values=sapply(p, entropy)
entropy_values
