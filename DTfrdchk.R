library(C50)
frdchk <- read.csv(file.choose())
View(frdchk)
attach(frdchk)
str(frdchk)

frdchk_train <- frdchk[1:420,]
frdchk_test <- frdchk[420:600,]

# Building model on training data 
frdchk5.0_train <- C5.0(frdchk_train[,-5],frdchk_train$Marital.Status)
plot(frdchk5.0_train)# Tree graph

# Training accuracy
pred_train <- predict(frdchk5.0_train,frdchk_train)

mean(frdchk_train$Marital.Status==pred_train) # 100% Accuracy

library(caret)
confusionMatrix(pred_train,frdchk_train$Marital.Status)


predc5.0_test <- predict(frdchk5.0_train,newdata=frdchk_test) # predicting on test data
mean(predc5.0_test==frdchk_test$Marital.Status) # 100% accuracy 
confusionMatrix(predc5.0_test,frdchk_test$Marital.Status)
library(gmodels)
# Cross tablez
CrossTable(frdchk_test$Marital.Status,predc5.0_test) #100% accuracy



##### Using tree function 
library(tree)
# Building a model on training data 
frdchk_tree <- tree(frdchk_train$Marital.Status~.,data=frdchk_train) #tree function
plot(frdchk_tree) #for plotting 
text(frdchk_tree,pretty = 0) # for the text to include

# Predicting the test data using the model
pred_tree <- as.data.frame(predict(frdchk_tree,newdata=frdchk_test))
pred_tree["final"] <- NULL
pred_test_df <- predict(frdchk_tree,newdata=frdchk_test)


pred_tree$final <- colnames(pred_test_df)[apply(pred_test_df,1,which.max)]


mean(pred_tree$final==frdchk_test$Marital.Status) # Accuracy = 33.14%
CrossTable(frdchk_test$Marital.Status,pred_tree$final) #59/181 accuracy from crosstable for true positives and true negatives 


