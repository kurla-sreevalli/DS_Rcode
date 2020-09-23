library(C50)
company_data <- read.csv(file.choose())
View(company_data)
attach(company_data)
str(company_data)

company_data_train <- company_data[1:280,]
company_data_test <- company_data[281:400,]

# Building model on training data 
company_data5.0_train <- C5.0(company_data_train[,-5],company_data_train$ShelveLoc)
plot(company_data5.0_train)# Tree graph

# Training accuracy
pred_train <- predict(company_data5.0_train,company_data_train)

mean(company_data_train$ShelveLoc==pred_train) # 100% Accuracy

library(caret)
confusionMatrix(pred_train,company_data_train$ShelveLoc)

predc5.0_test <- predict(company_data5.0_train,newdata=company_data_test) # predicting on test data
mean(predc5.0_test==company_data_test$ShelveLoc) # 100% accuracy 
confusionMatrix(predc5.0_test,company_data_test$ShelveLoc)
library(gmodels)
# Cross tablez
CrossTable(company_data_test$ShelveLoc,predc5.0_test) #100% accuracy

##### Using tree function 
library(tree)
# Building a model on training data 
company_data_tree <- tree(company_data_train$ShelveLoc~.,data=company_data_train) #tree function
plot(company_data_tree) #for plotting 
text(company_data_tree,pretty = 0) # for the text to include

# Predicting the test data using the model
pred_tree <- as.data.frame(predict(company_data_tree,newdata=company_data_test))
pred_tree["final"] <- NULL
pred_test_df <- predict(company_data_tree,newdata=company_data_test)


pred_tree$final <- colnames(pred_test_df)[apply(pred_test_df,1,which.max)]


mean(pred_tree$final==company_data_test$ShelveLoc) # Accuracy = 61.66%
CrossTable(company_data_test$ShelveLoc,pred_tree$final) #74/120 accuracy from crosstable for true positives and true negatives 

