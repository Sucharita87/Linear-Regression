#Calories_consumed-> predict weight gained using calories consumed

library(readr)
calories<-read.csv("F://ExcelR//Assignment//Linear regression//calories_consumed.csv")
View(calories)
attach(calories)
plot(Wg,cc) # positive relation, almost linear
cor(cc,Wg) # corelation coefficient= 0.94 which depictsstrong relationship # Best model
reg<-lm(cc~Wg)
summary(reg) # Rsquare value = 0.8968
reg$fitted.values
reg$residuals
confint(reg,level=0.95)
predict(reg, interval="predict")

#log model Rsquare value= 0.8776
reg_log<-lm(cc~log(Wg))
summary(reg_log)

#exponential model Rsquare value= 0.7071
reg_exp(lm(log(cc)~Wg))
summary(reg_exp)

# we consider the first model with Rsquare value= 0.8968 as teh best model and given below are the Predicted weight gain values:

> predict(reg, interval="predict")

fit      lwr      upr
1  1807.718 1229.249 2386.187
2  2004.085 1432.376 2575.795
3  3498.181 2879.564 4116.799
4  2004.085 1432.376 2575.795
5  2217.528 1649.740 2785.316
6  1811.987 1233.697 2390.278
7  1850.407 1273.660 2427.154
8  1709.535 1126.585 2292.485
9  2857.854 2280.041 3435.668
10 3925.066 3264.781 4585.351
11 1790.643 1211.447 2369.839
12 1897.364 1322.350 2472.378
13 2324.249 1757.059 2891.439
14 3071.297 2483.085 3659.509




