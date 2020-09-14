ksv <- read.csv(file.choose())
View(ksv)
attach(ksv)

#all the bisuness moments included here
mean(ksv$Delivery.Time)
mean(ksv$Sorting.Time)
median(ksv$Delivery.Time)
median(ksv$Sorting.Time)
range(ksv$Delivery.Time)
range(ksv$Sorting.Time)
var(ksv$Delivery.Time)
var(ksv$Sorting.Time)
sd(ksv$Delivery.Time)
sd(ksv$Sorting.Time)

#for skewness and kurtosis
library(moments)
skewness(ksv)
kurtosis(ksv)

# plots
barplot(ksv$Delivery.Time)
barplot(ksv$Sorting.Time)
hist(ksv$Delivery.Time)
hist(ksv$Sorting.Time)
boxplot(ksv$Delivery.Time)
boxplot(ksv$Sorting.Time)
plot(ksv)

# Correlation coefficient value for delivery time and sorting time
cor(ksv)
ksv <- lm(Delivery.Time~Sorting.Time)
summary(ksv)
confint(ksv,level = 0.95)
predict(ksv,interval = "predict")
# R-squared value for the above model is 0.6823 
# we may have to do transformation of variables for better R-squared value
# Applying transformations

# Logarthmic transformation
ksv_log<-lm(Delivery.Time~log(Sorting.Time))  # Regression using logarthmic transformation
summary(ksv_log)
confint(ksv_log,level = 0.95)
predict(ksv_log,interval = "predict")
# R-squared value for the above model is 0.6954 
# we may have to do different transformation better R-squared value
# Applying different transformations

# Exponential model 
ksv_exp<-lm(log(Delivery.Time)~Sorting.Time) # regression using Exponential model
summary(ksv_exp)
confint(ksv_exp,level = 0.95)
predict(ksv_exp,interval = "predict")
# R-squared value for the above model is 0.7109
# we may have to do different transformation better R-squared value
# Applying different transformations

#square root transformation
ksv_sqrt <- lm(sqrt(Delivery.Time)~Sorting.Time)
summary(ksv_sqrt)
# R-squared value for the above model is 0.704


#comparing all the R-squared values we can see the exponential transformation model have the high R-squared value i.e., 0.7109
#for delivary time and sorting time