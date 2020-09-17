cud <- read.csv(file.choose())
View(cud)
attach(cud)

#all the bisuness moments included here
mean(cud$YearsExperience)
mean(cud$Salary)
median(cud$YearsExperience)
median(cud$Salary)
range(cud$YearsExperience)
range(cud$Salary)
var(cud$YearsExperience)
var(cud$Salary)
sd(cud$YearsExperience)
sd(cud$Salary)

#for skewness and kurtosis
library(moments)
skewness(cud)
kurtosis(cud)

# plots
barplot(cud$YearsExperience)
barplot(cud$Salary)
hist(cud$YearsExperience)
hist(cud$Salary)
boxplot(cud$YearsExperience)
boxplot(cud$Salary)
plot(cud)
# Correlation coefficient value for salary and years of experiance
cor(cud)
cud <- lm(Salary~ YearsExperience)
summary(cud)
confint(cud,level = 0.95)
predict(cud,interval = "predict")
# R-squared value for the above model is 0.957
# we may have to do transformation of variables for better R-squared value
# Applying transformations

# Logarthmic transformation
cud_log <- lm(Salary~log(YearsExperience))
summary(cud_log)
confint(cud_log,level = 0.95)
predict(cud,interval = "predict")
# R-squared value for the above model is 0.8539
# we may have to do different transformation better R-squared value
# Applying different transformations

# Exponential model 
cud_exp <- lm(log(Salary)~YearsExperience)
summary(cud_exp)
confint(cud_exp,level = 0.95)
predict(cud_exp,interval = "predict")
# R-squared value for the above model is 0.932
# we may have to do different transformation better R-squared value
# Applying different transformations

#square root transformation
cud_sqrt <- lm(sqrt(Salary)~YearsExperience)
summary(cud_sqrt)
confint(cud_sqrt,level = 0.95)
predict(cud_sqrt,interval = "predict")
# R-squared value for the above model is 0.9498
# we may have to do different transformation better R-squared value
# Applying different transformations

#square root transformation
cud_sqrt <- lm(Salary~sqrt(YearsExperience))
summary(cud_sqrt)
confint(cud_sqrt,level+0.95)
predict(cud_sqrt,interval = "predict")
# R-squared value for the above model is 0.931

#comparing all the R-squared values we can see that without applying transformations we got the high R-squared value i.e., 0.957
#for salary and years experience