xy <- read.csv(file.choose()) # choose the xy.csv data set
View(xy)
attach(xy)

#all the business moments included here
mean(xy$Weight.gained..grams.)
mean(xy$Calories.Consumed)
median(xy$Weight.gained..grams.)
median(xy$Calories.Consumed)
range(xy$Weight.gained..grams.)
range(xy$Calories.Consumed)
var(xy$Weight.gained..grams.)
var(xy$Calories.Consumed)
sd(xy$Weight.gained..grams.)
sd(xy$Calories.Consumed)

#for skewness and kurtosis
library(moments)
skewness(xy)
kurtosis(xy)

# plots
barplot(xy$Weight.gained..grams.)
barplot(xy$Calories.Consumed)
hist(xy$Weight.gained..grams.)
hist(xy$Calories.Consumed)
boxplot(xy$Weight.gained..grams.)
boxplot(xy$Calories.Consumed)
plot(xy)
# correlation co-efficient value for weight gained and calories consumed
cor(xy)
xy <- lm(Weight.gained..grams.~ Calories.Consumed)
summary(xy)
confint(xy, level = 0.95)
predict(xy,interval="predict")
# R - squared value for the above model is 0.8968
# we may have to do transformations of variables for better R- squared value
# Applying Transformations

# Logarthmic transformations
xy_log <- lm(Weight.gained..grams.~log(Calories.Consumed)) # Regression using logarthmic transformation
summary(xy_log)
confint(xy_log,level = 0.95)
predict(xy, interval = "predict")
# R- squared value for the above model is 0.8077 which means decreased 
# we may have to do different transformation for better R-squared value
# Applying different transformation

# Exponential transformation
xy_exp <- lm(log(Weight.gained..grams.)~Calories.Consumed) # regression using Exponential model
summary(xy_exp)
confint(xy_exp,level = 0.95)
predict(xy_exp, interval = "predict")
# R - squared value for the above model is 0.8776
# still we can consider a different transformation for getting a better R- squared value
# applying square root transformation

# square root transformation
xy <- lm(sqrt(Weight.gained..grams.)~ Calories.Consumed) # regression using square root model
summary(xy)
# R- squared value has increased from 0.8776 to 0.9139
# Higher the R-sqaured value - Better chances of getting good model 

# when we compare all the R-squared values including the R- squared values got from transformation we have the highest R- squared value as 0.9139 from the square root transformation model
# for weight gained and calories consumed