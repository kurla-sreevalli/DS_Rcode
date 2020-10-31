wines <- read.csv(file.choose())
View(wines)

help(princomp) ## to understand the api for princomp


#Considering only numerical values for applying PCA

attach(wines)
cor(wines)
# cor = TRUE use correlation matrix for getting PCA scores

pcaObj<-princomp(wines, cor = TRUE, scores = TRUE, covmat = NULL)

str(pcaObj)
## princomp(mydata, cor = TRUE) not_same_as prcomp(mydata, scale=TRUE); similar, but different
summary(pcaObj)
loadings(pcaObj)

plot(pcaObj) # graph showing importance of principal components 
# Comp.1 having highest importance (highest variance)

biplot(pcaObj)

# Showing the increase of variance with considering principal components
# Which helps in choosing number of principal components
plot(cumsum(pcaObj$sdev*pcaObj$sdev)*100/(sum(pcaObj$sdev*pcaObj$sdev)),type="b")
#pcaObj$loadings

pcaObj$scores[,1:3] # Top 3 PCA Scores which represents the whole data



# cbind used to bind the data in column wise
# Considering top 3 principal component scores and binding them with mydata
winesdata<-cbind(wines,pcaObj$scores[,1:3])
View(winesdata)

# preparing data for clustering (considering only pca scores as they represent the entire data)
clus_data<-winesdata[,8:10]

# Normalizing the data 
norm_clus<-scale(clus_data) # Scale function is used to normalize data
dist1<-dist(norm_clus,method = "euclidean") # method for finding the distance
# here I am considering Euclidean distance

# Clustering the data using hclust function --> Hierarchical
fit1<-hclust(dist1,method="complete") # method here is complete linkage

plot(fit1) # Displaying Dendrogram
plot(fit1,hang = -1)#hang =-1 used for allignment
groups<-cutree(fit1,4) # Cutting the dendrogram for 5 clusters
rect.hclust(fit1, k=4, border="red")
membership_1<-as.matrix(groups) # cluster numbering 

View(membership_1)

final1<-cbind(membership_1,winesdata) # binding column wise with orginal data
View(final1)
View(aggregate(final1[,-c(2,9:11)],by=list(membership_1),FUN=mean)) # Inferences can be
# drawn from the aggregate of the wines data on membership_1

write.csv(final1,file="winesmenbership.csv",row.names = F,col.names = F)
getwd()


#kmeans

wine_data <- read.csv(file.choose())
View(wine_data)
attach(wine_data)
input <- wine_data
View(input)

#imported the data for the above

#excluding the X name column before normalizing
normalized_data <- scale(input[,1:14])
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

fitwine <- kmeans(normalized_data, 2) # 2 cluster solution
str(fitwine)
fitwine$cluster
finalwine<- data.frame(input, fitwine$cluster) # append cluster membership
View(finalwine)
library(data.table)
setcolorder(finalwine, neworder = c("fitwine.cluster"))
View(finalwine)
aggregate(input[,1:14], by=list(fitwine$cluster), FUN=mean)
fitwine$size
