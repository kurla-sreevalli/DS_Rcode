library(readr)
glass <- read_csv("C:/Users/sreevalli kurla ksv/Downloads/Excelr Data-20190131T150000Z-001/Excelr Data/Data Sets/KNN/glass.csv")
View(glass)
str(glass)

#table of type
table(glass$Type)

glassdf <- as.data.frame(glass)

# Replace types column names for 1,2,3,4,5,6,7 as type1,type2,type3,type4,type5,type6,type7. type is factor with 7 levels that is 1,2,3,4,5,6 and 7. We also replacing these seven enteries.
glassdf$Type <- factor(glassdf$Type, levels = c("1","2","3","4","5","6","7"), labels = c("type1","type2","type3","type4","type5","type6","type7"))

# table or proportation of enteries in the datasets. What % of entry is type1,type2,type3,type4,type5,type6 and type7

round(prop.table(table(glassdf$Type))*100,10)
summary(glassdf[c("RI","Na","Mg")])

#Create a function to normalize the data
norm <- function(x){ 
  return((x-min(x))/(max(x)-min(x)))
}
#test normalization
norm(c(1,2,3,4,5))
norm(c(10,20,30,40,50))

#Apply the normalization function to wbcd dataset
glass_n <- as.data.frame(lapply(glassdf[1:9], norm))
View(glass_n)

#create training and test datasets
glass_train <- glass_n[1:149,]
glass_test <- glass_n[150:214,]

#Get labels for training and test datasets

glass_train_labels <- glassdf[1:149,10]
glass_test_labels <- glassdf[150:214,10]


# Build a KNN model on taining dataset
library("class")
library("caret")
# Building the KNN model on training dataset and also need labels which we are including c1. Once we build the preduction model
# we have to test on test dataset
glass_pred <- knn(train = glass_train, test = glass_test, cl = glass_train_labels, k=21)
class(glass_train)
class(glass_test)
## Now evualuation the model performance

# install package gmodels
install.packages("gmodels")
library("gmodels")

# Create cross table of predicted and actual
CrossTable( x =  glass_test_labels, y = glass_pred)

