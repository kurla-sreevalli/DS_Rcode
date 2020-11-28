wc.at <- read.csv(file.choose()) # choose the wc-at.csv data set
View(wc.at)
attach(wc.at)
plot(wc.at)
# Correlation coefficient value for Waist and Addipose tissue
cor(wc.at)
reg<-lm(AT~Waist)
summary(reg)
confint(reg,level = 0.95)
predict(reg,inteval="predict")
# R-squared value for the above model is 0.667. 
# we may have to do transformation of variables for better R-squared value
# Applying transformations

# Logarthmic transformation
reg_log<-lm(AT~log(Waist))  # Regression using logarthmic transformation
summary(reg_log)
confint(reg_log,level=0.95)
predict(reg_log,interval="predict")
# R-squared value for the above model is 0.6723. 
# we may have to do different transformation better R-squared value
# Applying different transformations

# Exponential model 
reg_exp<-lm(log(AT)~Waist) # regression using Exponential model
summary(model3)
# R-squared value has increased from 0.67 to 0.7071 
# Higher the R-sqaured value - Better chances of getting good model 
# for Waist and addipose Tissue
