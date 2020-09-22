library(readxl)
CCsales <- read_excel(file.choose())
View(CCsales)
attach(CCsales)


#plot(CCsales$Sales...000.,type="o")
# So creating 4 dummy variables 
library(dummies)
X<- data.frame(outer(rep(Quarter,length = 42), Quarter,"==") + 0 )# Creating dummies for 4 quarters
View(X)

colnames(X)<-Quarter # Assigning month names 
View(X)
CCdata<-cbind(CCsales,X)
View(CCdata)
colnames(CCdata)[2]<-"SALES"
colnames(CCdata)
CCdata["t"]<- 1:42
View(CCdata)
CCdata["log_sales"]<-log(CCdata["SALES"])
CCdata["t_square"]<-CCdata["t"]*CCdata["t"]
attach(CCdata)
View(CCdata)
train<-CCdata[1:29,]

test<-CCdata[30:42,]

########################### LINEAR MODEL #############################

linear_model<-lm(SALES~t,data=train)
summary(linear_model)
linear_pred<-data.frame(predict(linear_model,interval='predict',newdata =test))
#linear_pred in console window
View(linear_pred)
rmse_linear<-sqrt(mean((test$SALES-linear_pred$fit)^2,na.rm = T))
rmse_linear # 799.0289



######################### Exponential #################################

expo_model<-lm(log_sales~t,data=train)
summary(expo_model)
expo_pred<-data.frame(predict(expo_model,interval='predict',newdata=test))
rmse_expo<-sqrt(mean((test$SALES-exp(expo_pred$fit))^2,na.rm = T))
rmse_expo # 637.5586

######################### Quadratic ####################################

Quad_model<-lm(SALES~t+t_square,data=train)
summary(Quad_model)
Quad_pred<-data.frame(predict(Quad_model,interval='predict',newdata=test))
rmse_Quad<-sqrt(mean((test$SALES-Quad_pred$fit)^2,na.rm=T))
rmse_Quad # 508.4168

######################### Additive Seasonality #########################

sea_add_model<-lm(SALES~Q1_86+Q2_86+Q3_86+Q4_86,data=train)
summary(sea_add_model)
sea_add_pred<-data.frame(predict(sea_add_model,newdata=test,interval='predict'))
rmse_sea_add<-sqrt(mean((test$SALES-sea_add_pred$fit)^2,na.rm = T))
rmse_sea_add # 1786.11

######################## Additive Seasonality with Linear #################

Add_sea_Linear_model<-lm(SALES~t+Q1_86+Q2_86+Q3_86+Q4_86,data=train)
summary(Add_sea_Linear_model)
Add_sea_Linear_pred<-data.frame(predict(Add_sea_Linear_model,interval='predict',newdata=test))
rmse_Add_sea_Linear<-sqrt(mean((test$SALES-Add_sea_Linear_pred$fit)^2,na.rm=T))
rmse_Add_sea_Linear # 600.9622

######################## Additive Seasonality with Quadratic #################

Add_sea_Quad_model<-lm(SALES~t+t_square+Q1_86+Q2_86+Q3_86+Q4_86,data=train)
summary(Add_sea_Quad_model)
Add_sea_Quad_pred<-data.frame(predict(Add_sea_Quad_model,interval='predict',newdata=test))
rmse_Add_sea_Quad<-sqrt(mean((test$SALES-Add_sea_Quad_pred$fit)^2,na.rm=T))
rmse_Add_sea_Quad # 393.9903

######################## Multiplicative Seasonality #########################

multi_sea_model<-lm(log_sales~Q1_86+Q2_86+Q3_86+Q4_86,data = train)
summary(multi_sea_model)
multi_sea_pred<-data.frame(predict(multi_sea_model,newdata=test,interval='predict'))
rmse_multi_sea<-sqrt(mean((test$SALES-exp(multi_sea_pred$fit))^2,na.rm = T))
rmse_multi_sea # 1839.316

######################## Multiplicative Seasonality Linear trend ##########################

multi_add_sea_model<-lm(log_sales~t+Q1_86+Q2_86+Q3_86+Q4_86,data = train)
summary(multi_add_sea_model) 
multi_add_sea_pred<-data.frame(predict(multi_add_sea_model,newdata=test,interval='predict'))
rmse_multi_add_sea<-sqrt(mean((test$SALES-exp(multi_add_sea_pred$fit))^2,na.rm = T))
rmse_multi_add_sea # 407.1372

# Preparing table on model and it's RMSE values 

table_rmse<-data.frame(c("rmse_linear","rmse_expo","rmse_Quad","rmse_sea_add","rmse_Add_sea_Quad","rmse_multi_sea","rmse_multi_add_sea"),c(rmse_linear,rmse_expo,rmse_Quad,rmse_sea_add,rmse_Add_sea_Quad,rmse_multi_sea,rmse_multi_add_sea))
View(table_rmse)
colnames(table_rmse)<-c("model","RMSE")
View(table_rmse)

# Additive seasonality with Quadratic has least RMSE value

new_model <- lm(SALES~t+t_square+Q1_86+Q2_86+Q3_86+Q4_86,data=CCdata)


resid <- residuals(new_model)
resid[1:10]
windows()
acf(resid,lag.max = 10)
# By principal of parcimony we will consider lag - 1  as we have so 
# many significant lags 
# Building Autoregressive model on residuals consider lag-1 

k <- arima(resid, order=c(1,0,0))
str(k)

View(data.frame(res=resid,newresid=k$residuals))
windows()
acf(k$residuals,lag.max = 15)
pred_res<- predict(arima(k$residuals,order=c(1,0,0)),n.ahead = 12)
str(pred_res)
pred_res$pred
acf(k$residuals)
write.csv(CCdata,file="ccdata.csv",col.names = F,row.names = F)
getwd()

####################### Predicting new data #############################
library(readxl) #ignore if have saved the trekdata in csv format.
test_data<-read.csv(file.choose())
View(test_data)
pred_new<-data.frame(predict(new_model,newdata=test_data,interval = 'predict'))
View(pred_new)
#pred_new$fit <- pred_new$fit +pred_res$pred
#View(pred_new)
#running the whole model for predicted data set 