startup <- read.csv(file.choose())
View(startup)
attach(startup)
summary(startup)

# Since column State has non numerical data, we are using Dummies library to transform this data into numeric:
library(dummies)
dummy(startup)
startup.new <- dummy.data.frame(startup,sep=".")
names(startup.new)
View(startup.new)

summary(startup.new)
#finding co-relation b/w output and input - SCATTER PLOT
pairs(startup.new)
# From the above, we learn that there is a strong correlation between Profit and R.D.Spend.
# Also a moderate correlation exists between R.D.Spend and Marketing.Spend.
qqnorm(Profit)
qqline(Profit)

# The qqnorm graph indicates data set profit is normally distributed.

hist(Profit)
# Histogram indicates data set Profit is negatively skewed.

# Correlation Coefficient matrix - Strength & Direction of Correlation
cor(startup.new)

### Partial Correlation matrix - Pure Correlation  b/n the varibles
library(corpcor)
cor2pcor(cor(startup.new)) #whether to check the influence of other variables on the correlated variables
#it says there is no dependency or influence between variables

# The Linear Model of Profit with all the columns
model.startup <- lm(Profit~R.D.Spend+Administration+Marketing.Spend,data = startup.new)
summary(model.startup)

# We have r squared value = 0.9507 with significant P-Value for R.D.Spend indicating a better model.
# Further our analysis as below:

# Multicollinearity check
# Model based on only R.D.Spend

model.startupRD <- lm(Profit~R.D.Spend,data = startup.new)
summary(model.startupRD)
# For R.D.Spend we have R-squared = 0.9465 with significant P-value indicating a better model

# Model based on only Administration
model.startupADMIN <- lm(Profit~Administration,data = startup.new)
summary(model.startupADMIN)
# For Administration we have R-squared = 0.04029 with not significant P-value

# Model based on only Markting Spend
model.startupMARK <- lm(Profit~Marketing.Spend,data = startup.new)
summary(model.startupMARK)
# For Administration we have R-squared = 0.5592 with significant P-value





### Scatter plot matrix along with Correlation Coefficients
panel.cor<-function(x,y,digits=2,prefix="",cex.cor)
{
  usr<- par("usr"); on.exit(par(usr))
  par(usr=c(0,1,0,1))
  r=(cor(x,y))
  txt<- format(c(r,0.123456789),digits=digits)[1]
  txt<- paste(prefix,txt,sep="")
  if(missing(cex.cor)) cex<-0.4/strwidth(txt)
  text(0.5,0.5,txt,cex=cex)
}
pairs(startup.new,upper.panel = panel.cor,main="Scatter plot matrix with Correlation coefficients")


library(psych)
pairs.panels(startup.new)
# It is Better to delete influential observations rather than deleting entire column which is 
# costliest process
# Deletion Diagnostics for identifying influential observations
influence.measures(model.startup)
library(car)
## plotting Influential measures 
influenceIndexPlot(model.startup,id.n=3) # index plots for infuence measures

influencePlot(model.startup,id.n=3) # A user friendly representation of the above





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


finalmodelStartUp<-lm(Profit~ R.D.Spend+log(Administration+Marketing.Spend),data=startup.new)
summary(finalmodelStartUp)
# The above model is best fit with R-squared value = 0.9508 and having all significant P-values.


# Evaluate model LINE assumptions 
plot(finalmodelStartUp)


qqPlot(finalmodelStartUp,id.n=5)

hist(residuals(finalmodelStartUp)) # close to normal distribution

