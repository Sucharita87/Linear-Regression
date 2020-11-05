#Delivery_time -> Predict delivery time using sorting time 

library(readr)
library(ggplot2)
library(stats)
delivery_time<-read.csv("F://ExcelR//Assignment//Linear regression//delivery_time.csv")
View(delivery_time)
attach(delivery_time)

boxplot(Delivery.Time)
boxplot(Sorting.Time)
dt<-Delivery.Time
st<-Sorting.Time
plot(st,dt) # positive relationship, but scattered
cor(st,dt) # corelation coefficient= 0.825 which depicts medium relationship

reg<-lm(dt~st) # linear model
summary(reg) # R square value= 0.6823, which is very low and we need to transform the data
confint(reg, level = 0.95)
predict(reg, interval = "predict")
pred_lm<-predict(reg)
reg$fitted.values
reg$residuals
sum(reg$residuals)
sqrt(sum(reg$residuals^2/nrow(delivery_time))) # RMSE= 2.79
ggplot(data=delivery_time, aes(x = st, y = dt)) +
       geom_point (color = "blue") + 
       geom_line (color = "red", data = delivery_time, aes(x= st, y = pred_lm))


reg_log<-lm(dt~log(st)) # log model
summary(reg_log) # R square value= 0.6954, which is a very minute improvement and we need to transform the data
confint(reg_log, level = 0.95)
predict(reg_log, interval = "predict")
pred_log <-predict(reg_log)
reg_log$fitted.values
reg_log$residuals
sum(reg_log$residuals)
sqrt(sum(reg_log$residuals^2/nrow(delivery_time))) # RMSE = 2.73
ggplot(data= delivery_time, aes(x=log(st), y = dt)) +
      geom_point(color = "green") +
      geom_line (color = "black", data = delivery_time, aes(x= log(st), y = pred_log))


reg_exp<-lm(log(dt)~st) # exp model
summary(reg_exp) # R square value= 0.7109, is less than 0.8 so we need to transform the data
confint(reg_exp, level = 0.95)
predict(reg_exp,interval = "predict")


reg_sqrt<-lm(Delivery.Time~sqrt(Sorting.Time)) # sqrt model
summary(reg_sqrt) # R square value= 0.6958, which is a very minute improvement and we need to transform the data
confint(reg_sqrt, level = 0.95)
predict(reg_sqrt,interval = "predict")


plot(st*st, dt) # polynomial model
cor (st*st, dt) # corelation coefficient = 0.79
reg_pol <- lm((dt ~ st*st))
summary(reg_pol) # R square value = 0.6823, no improvement

reg2degree<- lm(log(dt) ~ st + I(st*st)) # 2degree model
summary(reg2degree) # R square value = 0.7649, still less than 0.8
pred<-predict(reg2degree)
expy<-exp(pred)
err= delivery_time$Delivery.Time - expy
sqrt(sum(err^2)/nrow(delivery_time)) # RMSE = 2.79

confint(reg2degree,level=0.95)
predict(reg2degree,interval="confidence")

ggplot(data = delivery_time, aes(x = Sorting.Time + I(Sorting.Time^2), y = log(Delivery.Time))) + 
  geom_point(color='green') +
  geom_line(color='black',data = delivery_time, aes(Sorting.Time + I(Sorting.Time^2), y = pred))


reg3degree<- lm(log(Delivery.Time) ~ Sorting.Time + I(Sorting.Time^2) + I(Sorting.Time^3)) # 3degree model
summary(reg3degree) # R square value = 0.7819, still less than 0.8
pred1<-predict(reg3degree)
expy1<-exp(pred1)
err1= delivery_time$Delivery.Time - expy1
sqrt(sum(err1^2)/nrow(delivery_time)) # RMSE = 2.706, better value than 2degree

confint(reg3degree,level=0.95)
predict(reg3degree,interval="confidence")

ggplot(data = delivery_time, aes(x = st, y = dt)) + 
  geom_point(color='red') +
  geom_line(color='black',data = delivery_time, aes(x= Sorting.Time + I(Sorting.Time^2) + I(Sorting.Time^3), y = pred))

reg5<-lm(log(dt)~poly(st,5)) 
summary(reg5) # R square value = 0.7884
pred5<-predict(reg5)
ep5<-exp(pred5)
stnew= (poly(st,5))
err5= delivery_time$Delivery.Time - ep5
sqrt(sum(err5^2)/nrow(delivery_time)) # RMSE = 2.65, better value than 2,3 degree
confint(reg5, level= 0.95)
predict(reg5, interval = "confidence")
reg5$fitted.values
reg$residuals
sum(reg$residuals)
ggplot(data = delivery_time, aes(x= stnew, y = log(dt))) +
  geom_point(color= "blue") +
  geom_line(color="green", data = delivery_time, aes(x= stnew, y = pred5))

  