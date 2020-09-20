toyota <- read.csv(file.choose())
View(toyota)
toyota1 <- toyota[-1]
toyota2 <- toyota1[-1]
attach(toyota2)
summary(toyota2)

# Since column State has non numerical data, we are using Dummies library to transform this data into numeric:
library(dummies)
toyota.new <- dummy.data.frame(toyota2,sep=".")
names(toyota.new)
#View(toyota.new)
toyota.new1 <- toyota.new[-4]
#View(toyota.new1)
toyota.new2 <- toyota.new1[-7]
#View(toyota.new2)
toyota.new3 <- toyota.new2[-18]
#View(toyota.new3)
toyota.new4 <- toyota.new3[-21]
#View(toyota.new4)
summary(toyota.new4)


#finding co-relation b/w output and input - SCATTER PLOT
graphics.off()
par("mar")
par(mar=c(1,1,1,1))


#i'm not able to plot this after changing the graffics
#and its showing up an error :Error in plot.new() : figure margins too large
#please check this and help me
pairs(toyota.new4)
# From the above, we learn that there is a strong correlation between Profit and R.D.Spend.
# Also a moderate correlation exists between R.D.Spend and Marketing.Spend.
qqnorm(Price)
qqline(Price)


# The qqnorm graph indicates data set profit is normally distributed.

hist(Price)
# Histogram indicates data set Profit is positively skewed.

# Correlation Coefficient matrix - Strength & Direction of Correlation
cor(toyota.new4)

### Partial Correlation matrix - Pure Correlation  b/n the varibles
library(corpcor)
cor2pcor(cor(toyota.new4)) #whether to check the influence of other variables on the correlated variables

# The Linear Model of Profit with all the columns
model.toyota <- lm(Price~.,data = toyota.new4)
summary(model.toyota)

# We have r squared value = 0.9108 with significant P-Value for R.D.Spend indicating a better model.
# Further our analysis as below:

# Multicollinearity check


model.toyota1 <- lm(Price~Age_08_04,data = toyota.new4)
summary(model.toyota1)
# For R.D.Spend we have R-squared = 0.7684 with significant P-value indicating a better model

# Model based on only Administration
model.toyota2 <- lm(Price~toyota.new4$KM,data = toyota.new4)
summary(model.toyota2)
# For Administration we have R-squared = 0.3249 with not significant P-value

# Model based on only Markting Spend
model.toyota3 <- lm(Price~toyota.new4$HP,data = toyota.new4)
summary(model.toyota3)
# For Administration we have R-squared = 0.09922 with significant P-value





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
pairs(toyota.new4,upper.panel = panel.cor,main="Scatter plot matrix with Correlation coefficients")


library(psych)
pairs.panels(toyota.new4)
# It is Better to delete influential observations rather than deleting entire column which is 
# costliest process
# Deletion Diagnostics for identifying influential observations
influence.measures(model.toyota)
library(car)
## plotting Influential measures 
influenceIndexPlot(model.toyota,id.n=3) # index plots for infuence measures

influencePlot(model.toyota,id.n=3) # A user friendly representation of the above





# Variance Inflation Factor > 10 then there exists collinearity among all the variables 
vif(model.toyota)
# VIF values for all variables are < 10 hence no collinearity among the variables.

# Added Variable plot to check correlation b/n variables and o/p variable
avPlots(model.toyota,id.n=2,id.cex=0.7)



influence.measures(model.toyota)
library(car)
## plotting Influential measures 
influenceIndexPlot(model.toyota,id.n=3) # index plots for infuence measures
influencePlot(model.toyota,id.n=3) # A user friendly representation of the above
# No influential observations detected in the data set.


finalmodeltoyota<-lm(Price~ Age_08_04+log(toyota.new4$KM+toyota.new4$HP),data=toyota.new)
summary(finalmodeltoyota)
# The above model is best fit with R-squared value = 0.7854 and having all significant P-values.


# Evaluate model LINE assumptions 
plot(finalmodeltoyota)


qqPlot(finalmodeltoyota,id.n=5)

hist(residuals(finalmodeltoyota)) # close to normal distribution


