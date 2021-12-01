install.packages(c("ggplot2","lattice","caret","e1071"))
install.packages("MASS")
install.packages("caTools")

library(MASS)
library(caTools)
library(psych)
library(ROCR)

#pima.te= read.csv("..............")

data("Pima.te")
#pima<- Pima.te
View(Pima.te)

set.seed(15)

split<-sample.split(Pima.te,SplitRatio = 0.80)
#
split
training<-subset(Pima.te,split==TRUE)
View(training)
testing<-subset(Pima.te,split==FALSE)
View(testing)
id<-sample(2,nrow(Pima.te),prob = c(0.8,0.2),replace= TRUE)
id

training<-Pima.te[id==1,]
testing<-Pima.te[id==2,]

model=glm(type~.,training,family="binomial")
model
summary(model)
model$coefficients

res=predict(model,testing,type = "response")
table(Actualval=testing$type,predictval=res>0.3)

library(ROCR)
res4<-predict(model,training,type="response")
res4
ROCRPred <-prediction(res4,training$type)
ROCRPref<-performance(ROCRPred,"tpr","fpr")
plot(ROCRPref,colorize=TRUE,print.cutoffs.at=seq(0.1,by=0.1))



install.packages("rattle.data")
library(rattle.data)
# Loading the wine data
data(wine)
View(wine)

str(wine)

library(dplyr)

# Using sample_frac to create 70 - 30 slipt into test and train
train <- sample_frac(wine, 0.7)
sample_id <- as.numeric(rownames(train)) # rownames() returns character so as.numeric
test <- wine[-sample_id,]
View(test)


train$Type <- relevel(train$Type, ref = "3")
#Once the baseline has been specified, we use multinom() function to fit the model and then use summary() function to explore the beta coefficients of the model.

# Loading the nnet package
install.packages("nnet")
require(nnet)
# Training the multinomial model
multinom.fit <- multinom(Type ~ Alcohol + Color -1, data = train)
summary(multinom.fit)
exp(coef(multinom.fit))

head(probability.table=fitted(multinom.fit))

head(probability.table <- fitted(multinom.fit))

train$precticed <- predict(multinom.fit, newdata = train, "class")
ctable <- table(train$Type, train$precticed)



data(AirPassengers)
class(AirPassengers)
start(AirPassengers)
frequency(AirPassengers)
summary(AirPassengers)
plot(AirPassengers)
abline(reg=lm(AirPassengers~time(AirPassengers)))
boxplot(AirPassengers~cycle(AirPassengers))
plot(decompose(AirPassengers))


fit <- arima(log(AirPassengers), c(1, 1, 1),seasonal = list(order = c(1, 1, 1), period = 12))
pred <- predict(fit, n.ahead = 10*12)
pred
ts.plot(AirPassengers,pred$pred, log = "y", lty = c(1,3))
ts.plot(AirPassengers,2.718^pred$pred, log = "y", lty = c(1,3))
