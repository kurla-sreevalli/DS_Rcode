library(readr)
Zoo <- read_csv("C:/Users/sreevalli kurla ksv/Downloads/Excelr Data-20190131T150000Z-001/Excelr Data/Assignments/KNN/Zoo.csv")
class(Zoo)
View(Zoo)
str(Zoo)
#table of type 1->41, 2->20, 3->5, 4->13, 5->4, 6->8, 7->10
table(Zoo$type)

zoodf <- as.data.frame(Zoo)

# Replace types column names for 1,2,3,4,5,6,7 as type1,type2,type3,type4,type5,type6,type7. type is factor with 7 levels that is 1,2,3,4,5,6 and 7. We also replacing these seven enteries.
zoodf$type <- factor(zoodf$type, levels = c("1","2","3","4","5","6","7"), labels = c("type1","type2","type3","type4","type5","type6","type7"))

# table or proportation of enteries in the datasets. What % of entry is type1,type2,type3,type4,type5,type6 and type7

round(prop.table(table(zoodf$type))*100,18)
summary(zoodf[c("hair","feathers","eggs")])

#Create a function to normalize the data
norm <- function(x){ 
  return((x-min(x))/(max(x)-min(x)))
}
#test normalization
norm(c(1,2,3,4,5))
norm(c(10,20,30,40,50))

#Apply the normalization function to wbcd dataset
zoo_n <- as.data.frame(lapply(zoodf[2:17], norm))
View(zoo_n)

#create training and test datasets
zoo_train <- zoo_n[1:70,]
zoo_test <- zoo_n[71:101,]

#Get labels for training and test datasets

zoo_train_labels <- zoodf[1:70,18]
zoo_test_labels <- zoodf[71:101,18]


# Build a KNN model on taining dataset
library("class")
library("caret")
# Building the KNN model on training dataset and also need labels which we are including c1. Once we build the preduction model
# we have to test on test dataset
zoo_pred <- knn(train = zoo_train, test = zoo_test, cl = zoo_train_labels, k=21)
class(zoo_train)
class(zoo_test)
## Now evualuation the model performance


# install package gmodels
install.packages("gmodels")
library("gmodels")

# Create cross table of predicted and actual
CrossTable( x =  zoo_test_labels, y = zoo_pred)
