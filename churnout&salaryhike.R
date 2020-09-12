kvg <- read.csv(file.choose())
View(kvg)
attach(kvg)

#all the bisuness moments included here
mean(kvg$Salary_hike)
mean(kvg$Churn_out_rate)
median(kvg$Salary_hike)
median(kvg$Churn_out_rate)
range(kvg$Salary_hike)
range(kvg$Churn_out_rate)
var(kvg$Salary_hike)
var(kvg$Churn_out_rate)
sd(kvg$Salary_hike)
sd(kvg$Churn_out_rate)

#for skewness and kurtosis
library(moments)
skewness(kvg)
kurtosis(kvg)

# plots
barplot(kvg$Salary_hike)
barplot(kvg$Churn_out_rate)
hist(kvg$Salary_hike)
hist(kvg$Churn_out_rate)
boxplot(kvg$Salary_hike)
boxplot(kvg$Churn_out_rate)
plot(kvg)
# Correlation coefficient value for churnout rate and salary hike
cor(kvg)
kvg <- lm(Churn_out_rate~Salary_hike)
summary(kvg)
confint(kvg,level = 0.95)
predict(kvg,interval = "predict")
# R-squared value for the above model is 0.8312
# we may have to do transformation of variables for better R-squared value
# Applying transformations

# Logarthmic transformation
kvg_log <- lm(Churn_out_rate~log(Salary_hike))
summary(kvg_log)
confint(kvg_log,level = 0.95)
predict(kvg_log, interval = "predict")
# R-squared value for the above model is 0.8486
# we may have to do different transformation better R-squared value
# Applying different transformations

# Exponential model 
kvg_exp <- lm(log(Churn_out_rate)~Salary_hike)
summary(kvg_exp)
confint(kvg_exp,level = 0.95)
predict(kvg_exp,interval = "predict")
# R-squared value for the above model is 0.8735
# we may have to do different transformation better R-squared value
# Applying different transformations

#square root transformation
kvg_sqrt <- lm(sqrt(Churn_out_rate)~Salary_hike)
summary(kvg_sqrt)
confint(kvg_sqrt,level = 0.95)
predict(kvg_sqrt,interval = "predict")
# R-squared value for the above model is 0.853
# we may have to do different transformation better R-squared value
# Applying different transformations

#square root transformation
kvg_sqrt <- lm(Churn_out_rate~sqrt(Salary_hike))
summary(kvg_sqrt)
confint(kvg_sqrt,level = 0.95)
predict(kvg_sqrt,interval = "predict")
# R-squared value for the above model is 0.84

#comparing all the R-squared values we can see the exponential transformation model have the high R-squared value i.e., 0.8735
#for churn out rate and salary hike