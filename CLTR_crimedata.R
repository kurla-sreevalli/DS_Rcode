crime_data <- read.csv(file.choose())
View(crime_data)
attach(crime_data)
input <- crime_data
View(input)

#imported the data for the above

#excluding the X name column before normalizing
normalized_data <- scale(input[,2:5])
View(normalized_data)
d <- dist(normalized_data,method = "euclidean")# distance matrix
d

#model building
fitCD <- hclust(d,method = "complete")
fitCD
plot(fitCD)# display dendrogram
plot(fitCD,hang = -1)#hang =-1 used for allignment
groupsCD <- cutree(fitCD,k=5)# cut tree into 5 clusters
rect.hclust(fitCD, k=5, border="red")


membershipCD<-as.matrix(groupsCD)
table(membershipCD)

#combining the input data with the clusters for easy reference
finalCD <- data.frame(input, membershipCD)
View(finalCD)

#replacing the clusters column in first column of the data for easy analysis
finalCD1 <- finalCD[,c(ncol(finalCD),1:(ncol(finalCD)-1))]
View(finalCD1)

#save the finalCD1 for further analysis
write.csv(finalCD1,file = "finalCD1.csv")
#to know the current working directory getwd()
setwd("C:\\Users\\sreevalli kurla ksv\\Downloads\\Excelr Data-20190131T150000Z-001") #for future reference the working directory address


