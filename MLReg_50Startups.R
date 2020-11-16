# Multi Linear Regression - 50 Startups
Startups <- read.csv(file.choose())
attach(Startups)
View(Startups)
summary(Startups)

# Since column State has non numerical data, we are using Dummies library to transform this data into numeric:
library(dummies)
dummy(Startups)
Startups.new <- dummy.data.frame(Startups, sep = ".")
names(Startups.new)
# New data set will contain additional 3 columns one each for the name of the states:
View(Startups.new)

summary(Startups.new)

# Find the correlation b/n Output (Profit) & (R.D.Spend,VOL,SP)-Scatter plot
pairs(Startups.new)
# From the above, we learn that there is a strong correlation between Profit and R.D.Spend.
# Also a moderate correlation exists between R.D.Spend and Marketing.Spend.
qqnorm(Profit)
qqline(Profit)
v# The qqnorm graph indicates data set profit is normally distributed.
hist(Profit)
# Histogram indicates data set Profit is negatively skewed.

# Correlation Coefficient matrix - Strength & Direction of Correlation
cor(Startups.new)
# Correlation Coefficient matrix indicate that there is NO strong correlation among input variables and hence no collinearity problem among input variables.

### Partial Correlation matrix - Pure Correlation  b/n the varibles
#install.packages("corpcor")
library(corpcor)
cor2pcor(cor(Startups.new))
# No collenarity exists in the input variables.

# The Linear Model of Profit with all the columns
model.startup <- lm(Profit~R.D.Spend+Marketing.Spend+Administration,data=Startups.new)

summary(model.startup)
# We have r squared value = 0.9507 with significant P-Value for R.D.Spend indicating a better model.
# Further our analysis as below:

# Multicollinearity check
# Model based on only R.D.Spend
model.startupMulti<-lm(Profit~R.D.Spend,data=Startups.new)
summary(model.startupMulti) 
# For R.D.Spend we have R-squared = 0.9465 with significant P-value indicating a better model

# Model based on only Administration
model.startupAdm<-lm(Profit~Administration,data=Startups.new)
summary(model.startupAdm)
# For Administration we have R-squared = 0.04029 with not significant P-value

# Model based on only Markting Spend
model.startupMarket<-lm(Profit~Marketing.Spend,data=Startups.new)
summary(model.startupMarket)
# For Administration we have R-squared = 0.5592 with significant P-value

library(psych)
library(car)
# Variance Inflation Factor > 10 then there exists collinearity among all the variables 
vif(model.startup)
# VIF values for all variables are < 10 hence no collinearity among the variables.

# Added Variable plot to check correlation b/n variables and o/p variable
avPlots(model.startup,id.n=2,id.cex=0.7)
# Strong Positive correlation exists only between Profit and R.D Spend

influence.measures(model.startup)
library(car)
## plotting Influential measures 
influenceIndexPlot(model.startup,id.n=3) # index plots for infuence measures
influencePlot(model.startup,id.n=3) # A user friendly representation of the above
# No influential observations detected in the data set.

finalmodelStartUp<-lm(Profit~ R.D.Spend+log(Administration+Marketing.Spend),data=Startups.new)
summary(finalmodelStartUp)
# The above model is best fit with R-squared value = 0.9508 and having all significant P-values.

# Evaluate model LINE assumptions 
plot(finalmodelStartUp)

qqPlot(finalmodelStartUp,id.n=5)

hist(residuals(finalmodelStartUp)) # close to normal distribution

