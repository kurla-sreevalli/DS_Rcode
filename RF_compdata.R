library(randomForest)
comp <- read.csv(file.choose())
View(comp)

attach(comp)

comp_train <- comp[1:280,]
comp_test <- comp[281:400,]

# Building a random forest model on training data
comp_build <- randomForest(ShelveLoc~.,data = comp_train,na.action = na.roughfix,importance=TRUE)
# Training accuracy
mean(comp_train$ShelveLoc==predict(comp_build,comp_train))# 100% accuracy

# Prediction of train data
pred_train <- predict(comp_build,comp_train)
library(caret)


# Confusion Matrix
confusionMatrix(comp_train$ShelveLoc, pred_train)


# Predicting test data 
pred_test <- predict(comp_build,newdata=comp_test)
mean(pred_test==comp_test$ShelveLoc) # Accuracy = 60.8%


# Confusion Matrix 

confusionMatrix(comp_test$ShelveLoc, pred_test)

# Visualization 
plot(comp_build,lwd=2)
legend("topright", colnames(comp_build$err.rate),col=1:4,cex=0.8,fill=1:4)


acc_comp <- mean(comp$ShelveLoc==predict(comp_build))
acc_comp
varImpPlot(comp_build)
