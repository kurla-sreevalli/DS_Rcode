library(data.table)

bank_data <- read.csv(file.choose())
View(bank_data)
summary(bank_data)

str(bank_data)
attach(bank_data)
y_model <- glm(y ~ age +balance + duration + campaign + pdays + previous + factor(default) + factor(housing) + factor(loan)
               + factor(poutfailure) + factor(poutother) + factor(poutsuccess) + factor(poutunknown)
               + factor(con_cellular) + factor(con_telephone) + factor(con_unknown) + factor(divorced)
               + factor(married) + factor(single) + factor(joadmin.) + factor(joblue.collar) + factor(joentrepreneur)
               + factor(johousemaid) + factor(jomanagement) + factor(joretired) + factor(joself.employed) + factor(joservices)
               + factor(jostudent) + factor(jotechnician) + factor(jounemployed) + factor(jounknown), data = bank_data)
summary(y_model)

library(MASS)
library(car)

stepAIC(y_model)

prob_y <- as.data.frame(predict(y_model, type = c("response"), bank_data))

final_y <- cbind(bank_data, prob_y)

confusion_y <- table(prob_y>0.5, bank_data$y)

table(prob_y>0.5)

confusion_y
accuracy_y <- sum(diag(confusion_y)/sum(confusion_y))

accuracy_y

# So My model accuracy is 89.8% so I can consider this model
