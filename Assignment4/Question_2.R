URL <- "https://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data"
data <- read.table(URL,sep=",")
colnames(data) <- c("class","alcohol","malic_acid","ash","alcalinity","magnesium","total_phenols","flavanoids",
                    "nonfalvanoid","roanthocyanins","color_intensity","hue","OD280/OD315","proline")
#display top six rows
head(data)

#check the means of predictors
apply(data[,-1],2,mean)

#check the variance of the predictors
apply(data[,-1],2,var)

#using prcomp to perform PCA
output <- prcomp(data[,-1],scale=TRUE)
output$rotation

#biplot
biplot(output,scale=0)

library(corrplot)
M <- cor(data)
corrplot(M, method = "circle")

#calculating correlation between malic_acid and hue
cor(data$malic_acid,data$hue)
#from the above results it is clear that as one variable increases other variable decreases.

#screeplot
screeplot(output)
summary(output)

#calculating proportion of varianve for each principle component
variance <- output$sdev^2
pve <- variance/sum(variance)

#screenplot
par(mfrow=c(1,2))
plot(pve, xlab="Principal Component", ylab="Proportion of Variance Explained ",ylim=c(0,1),type='b')
plot(cumsum(pve), xlab="Principal Component ", ylab=" Cumulative Proportion of Variance Explained ",main="Screen Plot-2", ylim=c(0,1), type='b')

#Proportion of variance expalined by PC1 and PC2
temp<-pve[1:2]*100
temp
sum(temp)
#PC1 and PC2 has determined a total of 57% of variance.
