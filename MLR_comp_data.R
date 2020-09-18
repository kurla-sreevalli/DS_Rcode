Cdata <- read.csv(file.choose())
View(Cdata)
Cdata <- Cdata[-1]
View(Cdata)
attach(Cdata)
summary(Cdata)

# Since columns  have non numerical data, we are using Dummies library to transform this data into numeric:
library(dummies)
dummy(Cdata)
Cdata.new <- dummy.data.frame(Cdata,sep=".")
names(Cdata.new)
View(Cdata.new)

summary(Cdata.new)

#finding co-relation b/w output and input - SCATTER PLOT
pairs(Cdata.new)

qqnorm(price)
qqline(price)
# The qqnorm graph indicates data set price is normally distributed.

hist(price)
# Histogram indicates data set price is positively skewed.

# Correlation Coefficient matrix - Strength & Direction of Correlation
cor(Cdata.new)


### Partial Correlation matrix - Pure Correlation  b/n the varibles
library(corpcor)
cor2pcor(cor(Cdata.new))#whether to check the influence of other variables on the correlated variables
#it says there is no dependency or influence between variables

# The Linear Model of Profit with all the columns
model.cdata <- lm(price~.,data = Cdata.new)
summary(model.cdata)

# We have r squared value = 0.7756 with significant P-Value for all the data setsgiven indicating a better model.
# Further our analysis as below:

# Multicollinearity check
# Model based on only speed

model.cdataSPD <- lm(price~speed,data = Cdata.new)
summary(model.cdataSPD)
# For speed we have R-squared = 0.09059 with significant P-value indicating a better model

# Model based on only hd

model.cdataHD <- lm(price~ Cdata.new$hd,data = Cdata.new)
summary(model.cdataHD)
# For hd we have R-squared =0.1851  with significant P-value indicating a better model

#model based on only ram
model.cdataRAM <- lm(price~Cdata.new$ram)
summary(model.cdataRAM)
# For ram we have R-squared =0.3878  with significant P-value indicating a better model

#model based on only screen
model.cdataSCREEN <- lm(price~screen,data = Cdata.new)
summary(model.cdataSCREEN)
# For screen we have R-squared =0.08764  with significant P-value indicating a better model

#model based on only ads
model.cdataADS <- lm(price~ Cdata.new$ads)
summary(model.cdataADS)
# For ads we have R-squared =0.002975  with significant P-value indicating a better model

#model based on only trend
model.cdataTREND <- lm(price~trend,data = Cdata.new)
summary(model.cdataTREND)
# For trend we have R-squared =0.03999  with significant P-value indicating a better model



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
pairs(Cdata.new,upper.panel = panel.cor,main="Scatter plot matrix with Correlation coefficients")


library(psych)
pairs.panels(Cdata.new)
# It is Better to delete influential observations rather than deleting entire column which is 
# costliest process
# Deletion Diagnostics for identifying influential observations
influence.measures(model.cdata)
library(car)
## plotting Influential measures 
influenceIndexPlot(model.cdata,id.n=3) # index plots for infuence measures

influencePlot(model.cdata,id.n=3) # A user friendly representation of the above

# No influential observations detected in the data set.




# Variance Inflation Factor > 10 then there exists collinearity among all the variables 
vif(model.cdata)
# VIF values for all variables are < 10 hence no collinearity among the variables.

# Added Variable plot to check correlation b/n variables and o/p variable
avPlots(model.cdata,id.n=2,id.cex=0.7)




finalmodelcdata<-lm(price~(Cdata.new$speed+Cdata.new$hd+Cdata.new$ram+Cdata.new$screen+Cdata.new$ads+Cdata.new$trend))
summary(finalmodelcdata)
# The above model is best fit with R-squared value = 0.7123 and having all significant P-values.


# Evaluate model LINE assumptions 
plot(finalmodelcdata)

qqPlot(finalmodelcdata,id.n=5)

hist(residuals(finalmodelcdata)) # close to normal distribution


