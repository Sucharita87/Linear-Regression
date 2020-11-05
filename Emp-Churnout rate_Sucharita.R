#Emp_data -> Build a prediction model for Churn_out_rate 

library(readr)
library(ggplot2)
emp<-read.csv("F://ExcelR//Assignment//Linear regression//emp_data.csv")
View(emp)
attach(emp)
sh<-Salary_hike
cr<-Churn_out_rate

plot(sh,cr) # negative relationship
cor(sh,cr) # corelation coefficient  = -0.91 , strong relationship
reg<-lm(cr~sh)
summary(reg) # Rsquare value = 0.831, good value
confint(reg, predict = 0.95)
predict(reg, predict = "interval")
pred<-predict(reg)
reg$fitted.values
reg$residuals
sum(reg$residuals)
sqrt(sum(reg$residuals^2/nrow(emp))) # RMSE = 3.99
ggplot(data = emp, aes(x = sh, y = cr)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = emp, aes(x=sh, y=pred))

reg_log<-lm(sh~log(cr))
summary(reg_log) # Rsquare value = 0.8735
confint(reg_log, predict =0.95)
predict(reg, predict= " interval")
pred_log<-predict(reg_log)
reg_log$residuals
reg_log$fitted.values
sum(reg_log$residuals)
sqrt(sum(reg_log$residuals^2/nrow(emp))) #RMSE = 31.069, its higher than the simple linear model, so we discard this
ggplot(data = emp, aes(x = sh, y = log(cr))) + 
  geom_point(color='blue') +
  geom_line(color='red',data = emp, aes(x=sh, y=pred_log))


reg_exp<-lm(log(sh)~cr)
summary(reg_exp) # Rsquare value = 0.8486

