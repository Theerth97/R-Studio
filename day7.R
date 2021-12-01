getwd()
View(slr)
plot(slr$Advt,slr$Sales)
model=lm(slr$Sales~slr$Advt)
model

summary(model)
prd=predict(lm(slr$Sales~slr$Advt))
prd
slr$prediction=prd
View(slr)

error=residuals(lm(slr$Sales~slr$Advt))
error               
slr$error=error
View(slr)
x=cor(slr$Advt,slr$Sales)
x*x

hist(error)
plot(slr$Advt,slr$error,main="linearity")


plot(slr$`Observation no`,slr$error,main = "ind of err")

plot(slr$prediction,slr$error,main="cont err var")

View(mtcars)

car(mtcars1)
cor(mtcars)

model