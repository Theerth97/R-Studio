View(mtcars)
library(caTools)
sp=sample.split(mtcars,SplitRatio = 0.7)
sp
trdata=subset(mtcars, split=="TRUE")
trdata
testdata=subset(mtcars, split=="FALSE")
testdata
model=lm(mtcars$disp~mtcars$qsec)
model
mtcars$prediction=model
View(mtcars)
prd=predict(model)
prd
sampdata=data.frame(qsec=40)
answ=predict(model,sampdata,type = "response")
answ
mtcars$prediction=prd
View(mtcars)
summary(model)
mtcars$prediction=model
x=cor(mtcars$disp,mtcars$qsec)
x*x
plot(mtcars$disp,mtcars$qsec)
error=residuals(lm(mtcars$disp~mtcars$qsec))
error               
hist(error)
new.speeds= data.frame(speed=c(20,40,60))
predict(model,newdata = new.speeds)


View(insurance)
summary(insurance)
describe(insurance)
boxplot(insurance$age,insurance$charges)
cor(insurance$age,insurance$charges)
plot(insurance$age,insurance$charges)
plot(insurance$age,insurance$bmi)
cor(insurance$age,insurance$bmi)
model1=lm(insurance$age~insurance$charges)
model1
predict(model1)


y<-rnorm(1000)
z <- rep(NA,1000)
mynewdata= sample(c(y,z),10)
mynewdata

y<-rnorm(1000)
z <- rep(NA,1000)
mynewdata= sample(c(y,z),10)
mynewdata

boxplot(insurance$age~insurance$charges, subset = (insurance$smoker== "yes"), 
        xlab = "age", ylab = "charges", main = "smoker")
boxplot(insurance$age~insurance$charges, subset = (insurance$smoker== "no"), 
        xlab = "age", ylab = "charges", main = "non smoker")
insurance_aov2 <- aov(insurance$age~factor(insurance$charges) * 
                     factor(insurance$smoker))
summary(insurance_aov2)

