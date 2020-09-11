library(data.table)

election_data <- read.csv(file.choose())
View(election_data)
#setkey(election_data,`Election-id`)
summary(election_data)

colnames(election_data)
plot(election_data)
attach(election_data)
election_response <- glm(Result ~ Year+Amount.Spent+Popularity.Rank, data = election_data)
summary(election_response)

# Residual Deviance is less than Null Deviance that's mean input variable are significance.

library(MASS)
stepAIC(election_response) # Checking best fit model

library(car)

vif(election_response)

exp(coef(election_response))

# Creating COnfusion matrix to check the accuracy

prob <- as.data.frame(predict(election_response, type = c("response"), election_data))

final <- cbind(election_data,prob)

confusion <- table(prob>0.5, election_data$Result)
table(prob>0.5)

confusion

Accuracy <- sum(diag(confusion)/sum(confusion))

Accuracy

# So My model accuracy is 90% so I can consider this model
