EastWestAirlines<-read_excel(file.choose())
View(EastWestAirlines)
norm_airliness<-scale(EastWestAirlines[,-1])
View(norm_airliness)
#hclust
distance<-dist(norm_airliness,method = "euclidean")
hclust_airlines<-hclust(distance,method = "single")
hclust_airlines1<-hclust(distance,method = "complete")

plot(hclust_airlines,hang = -1)
plot(hclust_airlines1,hang = -1)

group<-cutree(hclust_airlines,k=5)
hcluster<-as.matrix(group)
hclust_airliness_final<-data.frame(EastWestAirlines,hcluster)
View(hclust_airliness_final)


#kmeans
fviz_nbclust(norm_airliness,kmeans,method = "wss")
#k value =5

kmeans_airlines<-kmeans(norm_airliness,5)
library(animation)
kmeans.ani(norm_airliness,5)

groups_kmeans<-as.matrix(kmeans_airlines$cluster)
airliness_kmeans_final<-data.frame(EastWestAirlines,groups_kmeans)
View(airliness_kmeans_final)
