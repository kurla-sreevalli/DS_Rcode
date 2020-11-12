# Read the dataset
wbcd <- read.csv(file.choose())
class(wbcd)
View(wbcd)
#First colum in dataset is id which is not required so we will be taking out
wbcd <- wbcd[-1]
View(wbcd)
str(wbcd)
#table of diagonis B <- 357 and M <- 212
table(wbcd$diagnosis)

# Replace B with Benign and M with Malignant. Diagnosis is factor with 2 levels that is B and M. We also replacing these two entery with Benign and Malignat
wbcd$diagnosis <- factor(wbcd$diagnosis, levels = c("B","M"), labels = c("Benign","Malignant"))

# table or proportation of enteries in the datasets. What % of entry is Bengin and % of entry is Malignant
round(prop.table(table(wbcd$diagnosis))*100,1)
summary(wbcd[c("radius_mean","texture_mean","perimeter_mean")])
#Create a function to normalize the data
norm <- function(x){ 
  return((x-min(x))/(max(x)-min(x)))
}
#test normalization
norm(c(1,2,3,4,5))
norm(c(10,20,30,40,50))
#Apply the normalization function to wbcd dataset
wbcd_n <- as.data.frame(lapply(wbcd[2:31], norm))
View(wbcd_n)

#create training and test datasets
wbcd_train <- wbcd_n[1:469,]
wbcd_test <- wbcd_n[470:569,]

#Get labels for training and test datasets

wbcd_train_labels <- wbcd[1:469,1]
wbcd_test_labels <- wbcd[470:569,1]


# Build a KNN model on taining dataset
library("class")
library("caret")
# Building the KNN model on training dataset and also need labels which we are including c1. Once we build the preduction model
# we have to test on test dataset
wbcd_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=21)
class(wbcd_train)
class(wbcd_test)
## Now evualuation the model performance

# install package gmodels
install.packages("gmodels")
library("gmodels")

# Create cross table of predicted and actual
CrossTable( x =  wbcd_test_labels, y = wbcd_pred)
