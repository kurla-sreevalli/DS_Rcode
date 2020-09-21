library(readxl)
ALset <- read_excel(file.choose())
View(ALset)
attach(ALset)


#plot(ALset$Passengers...000.,type = "o")
# So creating 12 dummy variables 
library(dummies)
X<- data.frame(outer(rep(Quarter,length = 96), Quarter,"==") + 0 )# Creating dummies for 4 quarters
View(X)

colnames(X)<-Month # Assigning month names 
View(X)
ALdata<-cbind(ALset,X)
View(ALdata)
colnames(ALdata)[2]<-"PASSENGERS"
colnames(ALdata)
ALdata["t"]<- 1:96
View(ALdata)
ALdata["log_passengers"]<-log(ALdata["PASSENGERS"])
ALdata["t_square"]<-ALdata["t"]*ALdata["t"]
attach(ALdata)
View(ALdata)
train<-ALdata[1:67,]

test<-ALdata[68:96,]

########################### LINEAR MODEL #############################

linear_model<-lm(PASSENGERS~t,data=train)
summary(linear_model)
linear_pred<-data.frame(predict(linear_model,interval='predict',newdata =test))
#linear_pred in console window
View(linear_pred)
rmse_linear<-sqrt(mean((test$PASSENGERS-linear_pred$fit)^2,na.rm = T))
rmse_linear # 47.56252



######################### Exponential #################################

expo_model<-lm(log_passengers~t,data=train)
summary(expo_model)
expo_pred<-data.frame(predict(expo_model,interval='predict',newdata=test))
rmse_expo<-sqrt(mean((test$PASSENGERS-exp(expo_pred$fit))^2,na.rm = T))
rmse_expo # 42.21818

######################### Quadratic ####################################

Quad_model<-lm(PASSENGERS~t+t_square,data=train)
summary(Quad_model)
Quad_pred<-data.frame(predict(Quad_model,interval='predict',newdata=test))
rmse_Quad<-sqrt(mean((test$PASSENGERS-Quad_pred$fit)^2,na.rm=T))
rmse_Quad # 43.09008

######################### Additive Seasonality #########################

sea_add_model<-lm(PASSENGERS~ X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X11+X12,data=train)
summary(sea_add_model)
sea_add_pred<-data.frame(predict(sea_add_model,newdata=test,interval='predict'))
rmse_sea_add<-sqrt(mean((test$PASSENGERS-sea_add_pred$fit)^2,na.rm = T))
rmse_sea_add # 129.9164

######################## Additive Seasonality with Linear #################

Add_sea_Linear_model<-lm(PASSENGERS~t+X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X11+X12,data=train)
summary(Add_sea_Linear_model)
Add_sea_Linear_pred<-data.frame(predict(Add_sea_Linear_model,interval='predict',newdata=test))
rmse_Add_sea_Linear<-sqrt(mean((test$PASSENGERS-Add_sea_Linear_pred$fit)^2,na.rm=T))
rmse_Add_sea_Linear # 45.5606

######################## Additive Seasonality with Quadratic #################

Add_sea_Quad_model<-lm(PASSENGERS~t+t_square+X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X11+X12,data=train)
summary(Add_sea_Quad_model)
Add_sea_Quad_pred<-data.frame(predict(Add_sea_Quad_model,interval='predict',newdata=test))
rmse_Add_sea_Quad<-sqrt(mean((test$PASSENGERS-Add_sea_Quad_pred$fit)^2,na.rm=T))
rmse_Add_sea_Quad # 44.41453

######################## Multiplicative Seasonality #########################

multi_sea_model<-lm(log_passengers~X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X11+X12,data = train)
summary(multi_sea_model)
multi_sea_pred<-data.frame(predict(multi_sea_model,newdata=test,interval='predict'))
rmse_multi_sea<-sqrt(mean((test$PASSENGERS-exp(multi_sea_pred$fit))^2,na.rm = T))
rmse_multi_sea # 134.6355

######################## Multiplicative Seasonality Linear trend ##########################

multi_add_sea_model<-lm(log_passengers~t+X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X11+X12,data = train)
summary(multi_add_sea_model) 
multi_add_sea_pred<-data.frame(predict(multi_add_sea_model,newdata=test,interval='predict'))
rmse_multi_add_sea<-sqrt(mean((test$PASSENGERS-exp(multi_add_sea_pred$fit))^2,na.rm = T))
rmse_multi_add_sea # 42.48108

# Preparing table on model and it's RMSE values 

table_rmse<-data.frame(c("rmse_linear","rmse_expo","rmse_Quad","rmse_sea_add","rmse_Add_sea_Quad","rmse_multi_sea","rmse_multi_add_sea"),c(rmse_linear,rmse_expo,rmse_Quad,rmse_sea_add,rmse_Add_sea_Quad,rmse_multi_sea,rmse_multi_add_sea))
View(table_rmse)
colnames(table_rmse)<-c("model","RMSE")
View(table_rmse)

# exponential seasonality has least RMSE value

new_model <- expo_model<-lm(log_passengers~t,data=train)


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
write.csv(ALdata,file="aldata.csv",col.names = F,row.names = F)
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