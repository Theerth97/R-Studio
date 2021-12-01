library(readxl)
Heatcase <- read_excel("D:/SKILL ENEBLE/R CLASS/data sets/final HeatEx_Usecase.xlsx")
View(Heatcase)                                                  
dim(Heatcase)
summary(Heatcase)
describe(Heatcase)
range(Heatcase)
heatcase2 <- Heatcase[,-1] 
View(heatcase2)
summary(heatcase2)
library(psych)
describe(heatcase2)
plot(heatcase2)
boxplot(heatcase2)
boxplot(heatcase2$`Crude Flow rate (kg/hr)`)
corr=as.data.frame(cor(heatcase2))
set.seed(31)
id=sample(2,nrow(heatcase2),prob = c(0.7,0.3),replace = TRUE)
id
heat_train=heatcase2[id==1,]
heat_test=heatcase2[id==2,]
View(heat_test)
View(heat_train)

model=lm(heat_train$`Fouling Resistance`~.,data=heat_train, singular.ok = TRUE)
model
alias(model)
library(car)
vif(model)
summary(model)


m1 = lm(heat_train$FoulingResistance~., data=heat_train)
summary(m1)
#library(car)
#vif(m1)

m11 = lm(heat_train$FoulingResistance~.-Hot_out_Cold_in -Hot_inCold_out-`Kero Temp decrease`-`Crude Temp Increase`-`Crude Flow rate`, data=heat_train)
summary(m11)

m11 = lm(heat_train$FoulingResistance~.-Hot_out_Cold_in -Hot_inCold_out-`Kero Temp decrease`-`Crude Temp Increase`-`Crude Flow rate`-`Kero Temp In`-`Kero Temp Out`, data=heat_train)
summary(m11)

m12 = lm(heat_train$FoulingResistance~.-Hot_out_Cold_in -Hot_inCold_out-`Kero Temp decrease`-`Crude Temp Increase`-`Crude Flow rate`-`Kero Temp In`-`Kero Temp Out`-`Furnace inlet temp`, data=heat_train)
summary(m12)


m14 = lm(heat_train$FoulingResistance~.-Hot_out_Cold_in -Hot_inCold_out-`Kero Temp decrease`-`Crude Temp Increase`-`Crude Flow rate`-`Kero Temp In`-`Kero Temp Out`-`Furnace inlet temp`-`Q MW`-`U transfer rate`, data=heat_train)
summary(m14)

test_res = predict(model,heat_test,type="response")
test$pred = (test_res)
plot(model)
plot(heat_test$FoulingResistance,test$pred,col="blue", xlab="actual", ylab="pred") #
abline(0,1) #y=x lines

library(ModelMetrics)
err = rmse(heat_test$pred,test$FoulingResistance) #test Rmse
test$residual = heat_test$pred-test$FoulingResistance
summary(test$residual)

hist(test$residual)

plot(heat_test$residual,test$pred)
plot(heat_test$residual,test$pred)

