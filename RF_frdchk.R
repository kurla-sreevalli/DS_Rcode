library(randomForest)
rand <- read.csv(file.choose())
View(rand)

attach(rand)

rand_train <- rand[1:420,]
rand_test <- rand[421:600,]

# Building a random forest model on training data
fit.forest <- randomForest(Marital.Status~.,data = rand_train,na.action = na.roughfix,importance=TRUE)
# Training accuracy
mean(rand_train$Marital.Status==predict(fit.forest,rand_train))# 100% accuracy

# Prediction of train data
pred_train <- predict(fit.forest,rand_train)
library(caret)


# Confusion Matrix
confusionMatrix(rand_train$Marital.Status, pred_train)


# Predicting test data 
pred_test <- predict(fit.forest,newdata=rand_test)
mean(pred_test==rand_test$Marital.Status) # Accuracy = 33.8%


# Confusion Matrix 

confusionMatrix(rand_test$Marital.Status, pred_test)

# Visualization 
plot(fit.forest,lwd=2)
legend("topright", colnames(fit.forest$err.rate),col=1:4,cex=0.8,fill=1:4)


acc_rand <- mean(rand$Marital.Status==predict(fit.forest))
acc_rand
varImpPlot(fit.forest)
