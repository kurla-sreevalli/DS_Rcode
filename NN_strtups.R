library(readr)
strtups <- read.csv(file.choose())
View(strtups)
str(strtups)
attach(strtups)

# Since column State has non numerical data, we are using Dummies library to transform this data into numeric:
library(dummies)
dummy(strtups)
strtups.new <- dummy.data.frame(strtups, sep = ".")
names(strtups.new)
# New data set will contain additional 3 columns one each for the name of the states:
View(strtups.new)

summary(strtups.new)




#normal_strtups<-scale(strtups)
## or 
normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}
strtups_norm<-as.data.frame(lapply(strtups.new,FUN=normalize))
summary(strtups_norm$Profit)
#summary(normal_strtups)
strtups_train<-strtups_norm[1:35,]
strtups_test<-strtups_norm[36:50,]

# Using multilayered feed forward nueral network
# package nueralnet
install.packages("neuralnet")
install.packages("nnet")
library(neuralnet)
library(nnet)

# Building model
strtups_model <- neuralnet(Profit~.,data = strtups_train)
str(strtups_model)
plot(strtups_model)

# SSE sum of squared errors . least SSE best model
# Evaluating model performance
# compute function to generate ouput for the model prepared
model_results <- compute(strtups_model,strtups_test[1:6])
predicted_strength <- model_results$net.result
predicted_strength
model_results$neurons
cor(predicted_strength,strtups_test$Profit)
plot(predicted_strength,strtups_test$Profit)
model_5<-neuralnet(Profit~.,data= strtups_norm,hidden = 5)
plot(model_5)
model_5_res<-compute(model_5,strtups_test[1:6])#columns
pred_strn_5<-model_5_res$net.result
cor(pred_strn_5,strtups_test$Profit)
plot(pred_strn_5,strtups_test$Profit)
# SSE has reduced and training steps had been increased as the number of nuerons 
# under hidden layer are increased

