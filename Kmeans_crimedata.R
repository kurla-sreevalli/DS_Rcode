crime_data <- read.csv(file.choose())
View(crime_data)
attach(crime_data)
input <- crime_data
View(input)

#imported the data for the above

#excluding the X name column before normalizing
normalized_data <- scale(input[,2:5])
View(normalized_data)


#elbow curve & k ~ sqrt(n/2) to decide the k value
wss = (nrow(normalized_data)-1)*sum(apply(normalized_data, 2, var))		 # Determine number of clusters by scree-plot 
for (i in 2:8) wss[i] = sum(kmeans(normalized_data, centers=i)$withinss)
plot(1:8, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")   # Look for an "elbow" in the scree plot #
title(sub = "K-Means Clustering Scree-Plot")


# selecting K for kmeans clustering using kselection
install.packages("kselection")
library(kselection)

k <- kselection(normalized_data[,], parallel = TRUE, k_threshold = 0.9, max_centers=12)
k

#from the above observations we can take k value as 2

library(animation)
kmeans.ani(normalized_data,2)

fitcrime <- kmeans(normalized_data, 2) # 2 cluster solution
str(fitcrime)
fitcrime$cluster
finalcrime<- data.frame(input, fitcrime$cluster) # append cluster membership
View(finalcrime)
library(data.table)
setcolorder(finalcrime, neworder = c("fitcrime.cluster"))
View(finalcrime)
aggregate(input[,2:5], by=list(fitcrime$cluster), FUN=mean)
fitcrime$size
