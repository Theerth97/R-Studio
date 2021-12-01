library(readr)
telecom <- read_csv("C:/Users/ADMIN/Downloads/WA_Fn-UseC_-Telco-Customer-Churn.csv")
View(telecom)
dim(telecom)
summary(telecom)
describe(telecom)
range(telecom)
sum(is.na(telecom))
colSums(is.na(telecom)) 

telecom$TotalCharges[which(is.na(telecom$TotalCharges))]=mean(telecom$TotalCharges,na.rm = TRUE)
sum(is.na(telecom))
view(telecom)
sum(is.na(telecom))
dim(telecom)
library(plyr)
telecom$Churn=revalue(telecom$Churn,c("Yes"=1))
telecom$Churn=revalue(telecom$Churn,c("No"=0))
telecom$customerID=NULL
telecom$gender=NULL
telecom$SeniorCitizen=NULL
telecom$Partner=NULL
telecom$Dependents=NULL

View(telecom)


set.seed(20)
id=sample(2,nrow(telecom),prob = c(0.7,0.3),replace = TRUE)
id
teletrain=telecom[id==1,]
teletest=telecom[id==2,]
View(teletrain)
View(teletest)

model=lm(teletrain$Churn~.,teletrain)
model
summary(model)

model1 <- lm(teletrain$Churn ~ teletrain$tenure + teletrain$Contract + teletrain$PaperlessBilling + teletrain$TotalCharges + teletrain$InternetService, data = teletrain,family = binomial(link="logit"))
model1
summary(model1)
anova(model, test = 'Chisq')
anova(model1, test = 'Chisq')
anova(model,model1,test = "Chisq")

res=predict(model1,teletest,type = "response")
res2=predict.glm(model1,teletest,type = "response")
table(Actualval=teletrain$Churn,predictval=res>0.4)
table(Actualval=teletrain$Churn,predictval=res2>0.4)

library(randomForest)
set.seed(2017)
rfModel <- randomForest(Churn ~.,teletrain)
print(rfModel)
pred_rf <- predict(rfModel, teletest)
caret::confusionMatrix(pred_rf, teletest$Churn)